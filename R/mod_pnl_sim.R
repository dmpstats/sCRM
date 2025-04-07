#' pnl_sim UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pnl_sim_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    
    # Simulation options ------------------------------------------------------
    shinydashboardPlus::box(
      title = "Simulation", 
      width = 2,
      status = "primary",
      solidHeader = TRUE,
      gradient = TRUE, 
      background = "gray",
      
      dropdownMenu = info_dropdown(
        inputId = ns("siminfo"),
        placement = "bottom-start",
        #md_path =  "inst/app/markdown/info_buttons_docs/simulation_box.md"
        md_path =  app_sys("app/markdown/info_buttons_docs/simulation_box.md")
      ),
      
      mod_sim_options_ui(id = ns("sim-options"))
    ),
    
    
    # sCRM Outputs -------------------------------------------------------------
    
    # Only rendering box panel after simulation has finished
    uiOutput(outputId = ns("outputsbox"))
    
    
  )
}


#' pnl_sim Server Functions
#'
#' @import zeallot
#' @import stochLAB
#' @import waiter
#'
#' @noRd 
mod_pnl_sim_server <- function(id, scrm_inputs, band_mode, app_session){
  
  stopifnot(is.reactivevalues(scrm_inputs))
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # reactive value tracking if inputs and outputs are in-sync
    inout_synced <- reactiveVal()
    
    
    # Set progress screen for simulation --------------------------------------
    
    ## Create waiter to embed progress bars  ---------------------
    w <- Waiter$new(
      fadeout = TRUE,
      color = rgb(23, 26, 24, alpha = 200, maxColorValue = 255),
      html = tagList(
        h4("Calculating collisions"),
        br(),
        img(src = "www/wf_loading_3.gif", height = 120),
        rep_br(2),
        p("Scenarios under processing"),
        attendantBar(
          "spps-bar",
          width = 300,
          class = "scrm-run-progress"
        ),
        p("Total progress"),
        attendantBar(
          "wf-bar",
          width = 300,
          class = "scrm-run-progress"
        )
      )
    )
    
    ## Create attendants, one for each progress bar ---------------------
    att_spps <- Attendant$new("spps-bar")
    att_wf <- Attendant$new("wf-bar")
    
    
    # Generate table with input validation status per section of each scenario ---------
    iv_status <- reactive({

      inputs_dt <- reactiveValuesToList(scrm_inputs)
      
      if(inputs_dt$valid){

        # iterate over scenarios 
        purrr::map_dfr(inputs_dt$wf_scens[inputs_dt$active_scens], function(x){
          
          # only process when UI sections has been rendered
          if(not_null(x$valid)){
            if(not_null(x$wf_inputs)){

              dt <- data.frame(
                scenario =  x$wf_label, 
                section = "WF parameters",
                input_valid = x$wf_inputs$valid, 
                menu_item_id = "sbm-wf",
                tbox_id = "tbx-wf",
                tb_pnl_id = x$wf_inputs$wf_tp_id
              )
              
              # proceed if at least one species selected for current scenario
              if(x$valid){
               
                # iterate over spps in windfarm 
                spp_in_wf_dt <- purrr::map_dfr(
                  x$spp_in_wf[x$active_spps],
                  function(y){
                    data.frame(
                      section = y$spp_label,
                      input_valid = y$valid,
                      tbox_id = y$tbx_id,
                      tb_pnl_id = y$spp_tp_id
                    )
                  }) %>%
                  tibble::add_column(scenario = x$wf_label, .before = 1) %>%
                  tibble::add_column(menu_item_id = x$spp_wf_menu_id)
                
                # bind spp info with previous wf info
                dt <- dplyr::bind_rows(dt, spp_in_wf_dt)
                
              }else{
                #browser()
                dt <- dt %>%
                  tibble::add_row(
                    scenario =  x$wf_label, 
                    section = "Species selection",
                    input_valid = x$valid,
                    menu_item_id = x$spp_wf_menu_id, #"sbm-spp",
                    tbox_id = NA,
                    tb_pnl_id = NA
                  )
              }
              dt
            }
          }
        })

      }else{
        data.frame(
          scenario = "No scenarios specified",
          input_valid = FALSE
        )
      }
    })

    
    # Sub-modules server side --------------------------------------------------
    c(go_sim, rseed, niter) %<-% mod_sim_options_server(
      id = "sim-options",
      iv_status = iv_status,
      app_session = app_session,
      band_mode = band_mode
    )
    
    
    # Run sCRM -----------------------------------------------------------------
    scrm_outputs <- eventReactive(go_sim(), {
      
      # Initialize waiter
      w$show()
      
      output <- list()

      # total number of scenarios
      n_scen <- sum(purrr::map_int(scrm_inputs$wf_scens[scrm_inputs$active_scens], ~length(.$active_spps)))
      
      # initialize counter for scenario iteration
      i <- 0
      
      # Loop over wfs
      for(wf_id in scrm_inputs$active_scens){
        
        scen_inputs <- scrm_inputs$wf_scens[[wf_id]]
        
        wf_label <- scen_inputs$wf_label
        
        # unpack wf inputs for current wf
        c(..., wf_ftrs, trb_ftr, wf_oper) %<-% 
          scen_inputs$wf_inputs
        
        # number of species in current wf
        n_spps <- length(scen_inputs$active_spps)
        
        # initialize counter for spps in wf iteration
        j <- 0
        
        # Loop over spps in wf
        for(spp_id in scen_inputs$active_spps){
          
          # unpack current spp inputs for current wf
          c(., spp_label, ..., biom_ftrs, fhd, inflight_ftrs, bird_dens, c(out_opt, seas_dt)) %<-% 
            scen_inputs$spp_in_wf[[spp_id]]
          
          
          # Store wf and spp labels + output type
          output[[wf_id]][[spp_id]]$wf_label <- wf_label
          output[[wf_id]][[spp_id]]$spp_label <- spp_label
          output[[wf_id]][[spp_id]]$out_opt <- out_opt
          
          
          # Data prep for sCRM model
          
          # Model options and FHD type
          model_opt <- c(1, 2)
          
          if("gen_fhd" %in% fhd$fhd_type){
            
            model_opt <- c(model_opt, 3)
            
          }else{
            # Model Option 2 is *always* calculated in {sCRM} So, if generic fhd
            # is not specified, use site fhd to calculate option
            # 2. This is done by overwriting the generic data with the site data,
            # as `stochLAB::stoch_crm()` and `stochLAB::band_crm()` calculate
            # option 2 from the object assigned to, respectively, function
            # arguments `gen_fhd_boots` and `gen_fhd`
            fhd$gen_fhd <- fhd$site_fhd
          }
          
          if("site_fhd" %in% fhd$fhd_type){
            model_opt <- c(model_opt, 4)
          }
          
          # Prepare seasons data
          if(not_null(seas_dt)){
            season_dt <- seas_dt %>%
              dplyr::rename(season_id = period_name) %>%
              dplyr::mutate(across(c(start_month, end_month), .fns = substr, start = 1, stop = 3))
          }else{
            season_dt <- seas_dt  
          }
          
          # store season definition (if applicable)
          output[[wf_id]][[spp_id]]$season_dt <- season_dt
          
          # data.frames format
          wind_avbl <- wf_oper$wndavlb %>%
            dplyr::mutate(month = as.character(month)) %>%
            dplyr::rename(pctg = wndavlb) %>%
            tidyr::drop_na()
          
          dwnt_pars <- wf_oper$trbdwnt %>%
            dplyr::mutate(month = as.character(month)) %>%
            tidyr::drop_na()
          
          # increment scenario and spp counters
          i <- i + 1
          j <- j + 1
          
          # Update attendants
          att_spps$set(j/(n_spps*2) * 100, text = glue::glue("{spp_label} in {wf_label}"))
          tot_pctg <- i/(n_scen*2) * 100
          att_wf$set(tot_pctg, text = sprintf("%s%%", round(tot_pctg)))
          
          
          if(band_mode() == FALSE){
            
            # Data prep for stochastic model
            #
            # Bird density: translating type of data from shiny options to sCRM options
            dens_sampling <- switch(
              bird_dens$dens_type,
              tnorm = "tnorm",
              draws = "resample",
              pctls = "qtiles"
            )
            
            if(dens_sampling == "tnorm"){
              
              dens_dt <- bird_dens$dens_dt %>%
                dplyr::mutate(month = as.character(month)) %>%
                tidyr::drop_na()
              
            }else if(dens_sampling == "qtiles"){
              
              dens_dt <- bird_dens$dens_dt %>%
                dplyr::rename(p = pctl) %>%
                dplyr::mutate(p = 0.01*p)
              
            }else{
              dens_dt <- bird_dens$dens_dt
            }
            
            # Parameters treated as deterministic in {sCRM}, but are stochastic in {stochLAB}
            rtr_radius_pars <- data.frame(mean = trb_ftr$rtradius, sd = 0)
            air_gap_pars <- data.frame(mean = trb_ftr$airgap, sd = 0)
            bld_width_pars <- data.frame(mean = trb_ftr$bladewth, sd = 0)
            
            out_scrm <- stoch_crm(
              model_options = model_opt,
              n_iter = niter(),
              flt_speed_pars = inflight_ftrs$fl_speed,
              body_lt_pars = biom_ftrs$body_lt,
              wing_span_pars = biom_ftrs$wing_span,
              avoid_bsc_pars = inflight_ftrs$avoid_bsc,
              avoid_ext_pars = inflight_ftrs$avoid_ext,
              noct_act_pars = inflight_ftrs$nct_act,
              prop_crh_pars = inflight_ftrs$colrisk,
              bird_dens_opt = dens_sampling,
              bird_dens_dt = dens_dt,
              flight_type = inflight_ftrs$fltype,
              prop_upwind = inflight_ftrs$upwind,
              gen_fhd_boots = fhd$gen_fhd,
              site_fhd_boots = fhd$site_fhd,
              n_blades = trb_ftr$nblades,
              rtr_radius_pars = rtr_radius_pars,
              air_gap_pars = air_gap_pars,
              bld_width_pars = bld_width_pars,
              rtn_pitch_opt = trb_ftr$blade_pitch_rtn$opt,
              bld_pitch_pars = trb_ftr$blade_pitch_rtn$bld_pitch,
              rtn_speed_pars = trb_ftr$blade_pitch_rtn$rtn_speed,
              windspd_pars = trb_ftr$blade_pitch_rtn$wind_spd,
              rtn_pitch_windspd_dt = trb_ftr$blade_pitch_rtn$windspd_to_rtnspd_ptch,
              trb_wind_avbl = wind_avbl,
              trb_downtime_pars = dwnt_pars,
              wf_n_trbs = wf_ftrs$nturb,
              wf_width = wf_ftrs$wfwidth,
              wf_latitude = wf_ftrs$lat,
              tidal_offset = wf_ftrs$tdloffset,
              lrg_arr_corr = wf_ftrs$lac,
              verbose = FALSE,
              seed = rseed() %|NA|% NULL,
              out_format = "draws",
              out_sampled_pars = FALSE,
              out_period = out_opt,
              season_specs = season_dt,
              bld_chord_prf = stochLAB::chord_prof_5MW
            ) %>%
              purrr::map_dfr(function(x){
                tibble::as_tibble(x) %>%
                  tibble::add_column(iter = 1:nrow(.), .before = 1)
              },
              .id = "crm_option") %>%
              tidyr::pivot_longer(
                cols = -c(crm_option, iter),
                names_to = "time_period",
                values_to = "collisions")
            
            # force consistency among type of outputs
            if(out_opt == "annum"){
              
              out_scrm <- out_scrm %>%
                dplyr::mutate(
                  time_period = "Jan - Dec", 
                  aggr_label = "Annum", .before = 1)
              
            }else if(out_opt == "seasons"){
              
              out_scrm <- out_scrm %>%
                mutate(time_period = stringr::str_replace(time_period, "_", " - ")) %>%
                left_join(
                  ., 
                  tidyr::unite(season_dt, time_period, start_month, end_month, sep = " - "),
                  by = "time_period") %>%
                dplyr::relocate(season_id, .before = time_period) %>%
                rename(aggr_label = season_id)
              
            }else{
              out_scrm <- dplyr::mutate(out_scrm, aggr_label = "Month")
            }
            
            # store
            output[[wf_id]][[spp_id]]$colls <- out_scrm %>%
              dplyr::relocate(aggr_label, time_period, .before = crm_option)
            
          }else{
            
            # Data prep for deterministic model
            dens_dt <- bird_dens$dens_dt %>%
              dplyr::mutate(month = as.character(month)) %>%
              dplyr::rename(dens = Mean) %>%
              dplyr::select(month, dens)
            
            hub_height <- trb_ftr$airgap + trb_ftr$rtradius
            
            turb_oper <- dwnt_pars %>%
              left_join( wind_avbl, by = c("name", "month")) %>%
              mutate(
                prop_oper = 0.01 * (pctg - Mean)
              ) %>%
              dplyr::select(month, prop_oper)
            
            gen_fhd <- fhd$gen_fhd %>%
              dplyr::select(height, prop)
            
            out_band <- band_crm(
              model_options = model_opt,
              flight_speed = inflight_ftrs$fl_speed$mean,
              body_lt = biom_ftrs$body_lt$mean,
              wing_span = biom_ftrs$wing_span$mean,
              flight_type = inflight_ftrs$fltype,
              avoid_rt_basic = inflight_ftrs$avoid_bsc$mean,
              avoid_rt_ext = inflight_ftrs$avoid_ext$mean,
              noct_activity = inflight_ftrs$nct_act$mean,
              prop_crh_surv = inflight_ftrs$colrisk$mean,
              dens_month = dens_dt,
              prop_upwind = inflight_ftrs$upwind,
              gen_fhd = gen_fhd,
              site_fhd = fhd$site_fhd,
              rotor_speed = trb_ftr$blade_pitch_rtn$rtn_speed$mean,
              rotor_radius = trb_ftr$rtradius,
              blade_width = trb_ftr$bladewth,
              blade_pitch = trb_ftr$blade_pitch_rtn$bld_pitch$mean * pi/180,
              n_blades = trb_ftr$nblades,
              hub_height = hub_height,
              n_turbines = wf_ftrs$nturb,
              turb_oper_month = turb_oper,
              wf_width = wf_ftrs$wfwidth,
              wf_latitude = wf_ftrs$lat,
              tidal_offset = wf_ftrs$tdloffset,
              lrg_arr_corr = wf_ftrs$lac, 
              chord_prof = stochLAB::chord_prof_5MW
            )
            
            #browser()
            
            # currently stochLAB::band_crm doesn't feature outputs by season/annum.
            # So, doing it here
            if(out_opt == "annum"){
              
              out_band <- colSums(out_band[, -1])
              
              output[[wf_id]][[spp_id]]$colls <- tibble(
                aggr_label = "Annum",
                time_period = "Jan - Dec",
                crm_option = names(out_band),
                iter = 0,
                collisions = out_band
              )
              
            }else if(out_opt == "seasons"){  
              
              # Generate months-in-season support table
              seasons_months <- season_dt %>%
                dplyr::rowwise() %>%
                dplyr::mutate(
                  season_months = list(stochLAB::seq_months(start_month, end_month))
                ) %>%
                tidyr::unnest(season_months) %>%
                tidyr::unite(time_period, start_month, end_month, sep = " - ")
              
              # Calculate collision estimates per season period
              output[[wf_id]][[spp_id]]$colls <- out_band %>%
                dplyr::left_join(seasons_months, by = c("month" = "season_months")) %>%
                rename(aggr_label = season_id) %>%
                dplyr::group_by(aggr_label, time_period) %>%
                dplyr::summarise(dplyr::across(dplyr::contains("opt"), .fns = sum)) %>%
                tidyr::pivot_longer(
                  cols = !c(aggr_label, time_period),
                  values_to = "collisions",
                  names_to = "crm_option") %>%
                dplyr::mutate(iter = 0, .before = collisions)
              
            }else{
              
              output[[wf_id]][[spp_id]]$colls <- out_band %>%
                rename(time_period = month) %>%
                mutate(aggr_label = "Month", .before = 1) %>%
                tidyr::pivot_longer(
                  cols = !c(aggr_label, time_period),
                  values_to = "collisions",
                  names_to = "crm_option") %>%
                dplyr::mutate(iter = 0, .before = collisions) 
            }
          }
          
          # increment scenario and spp counters
          i <- i + 1
          j <- j + 1
          
          # Update attendants
          att_spps$set(j/(n_spps*2) * 100, text = glue::glue("{spp_label} in {wf_label}"))
          tot_pctg <- i/(n_scen*2) * 100
          att_wf$set(tot_pctg, text = sprintf("%s%%", round(tot_pctg)))
        }
      }
      
      # close waiter and attendants
      att_spps$done()
      att_wf$done()
      w$hide()
      
      # input and outputs are synced
      inout_synced(TRUE)
      
      output
    })
    
    
    # unsync inputs and outputs if any of the following objects changes
    observe({
      rvtl(scrm_inputs)
      niter()
      band_mode()
      inout_synced(FALSE)
    })
    
    
    # Render Outputs Panel, once simulation is finished ------------------------
    observeEvent(scrm_outputs(), {
    
      # Outputs module - ui side
      output$outputsbox <- renderUI({
        mod_sim_outputs_ui(
          id = ns("outputs"), 
          scrm_outputs = scrm_outputs(), 
          band_mode = band_mode()
          )
      })
      
      # Outputs module - server side
      mod_sim_outputs_server(
        id = "outputs",
        scrm_inputs = rvtl(scrm_inputs),
        scrm_outputs = scrm_outputs,
        band_mode = band_mode(),
        niter = niter(),
        inout_synced = inout_synced
      )
      
    })
    
  })
}
    
## To be copied in the UI
# mod_pnl_sim_ui("pnl_sim_ui_1")
    
## To be copied in the server
# mod_pnl_sim_server("pnl_sim_ui_1")