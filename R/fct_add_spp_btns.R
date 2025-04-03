#' Action buttons for adding and uploading species in windfarms (UI function)
#'
#' @description 
#' Inserts buttons controlling the addition of species and input uploading. It
#' should be inserted in the `title` argument of `shinydashboard::tabBox()`
#'   
#' @param slctize_id character string, the `inputId` of the dropdown menu with
#'   the selectize widget for adding species
#' @param dwnl_btn_id character string, the `inputId` of the download file widget
#' @param file_input_id character string, the `inputId` of the upload file widget
#'
#' @return No value returned
#' 
#' @noRd
add_spp_btns <- function(slctize_id, 
                         btn_add_id, 
                         drpdwn_btn_id, 
                         dwnl_btn_id, 
                         file_input_id,
                         dpdn_close_id,
                         wf_label){
  
  
  # Start-up species labels
  spp_choices <- sort(c(init_spp_label, "Arctic Skua", "Northern Fulmar", "Great Black-backed Gull",
                        "Common Guillemot", "Northern Gannet", "Black-legged Kittiwake",
                        "Lesser Black-Backed Gull", "Little Auk", "Atlantic Puffin",
                        "Razorbill", "Arctic Tern", "Black-headed Gull",
                        "Black-throated Diver", "Common Gull", "Common Scoter",
                        "Common Tern", "Cormorant", "Eider", "European Shag",
                        "Herring Gull", "Little Gull", "Manx Shearwater",
                        "Red-throated Diver", "Sandwich Tern"))
  
  
  # Add 2 demo species for dev mode
  if(golem::app_dev()){
    spp_choices <- c(spp_choices, "Demo Species 2", "Demo Species 3")
  }
  
  
  fluidRow(
    col_6(
      
      shinyWidgets::dropdown(
        inputId = btn_add_id,
        icon = icon("arrow-down-short-wide", verify_fa = FALSE), #icon("plus"),
        right = TRUE,
        status = "success",
        size = "sm",
        width = "450px",
        style = "material-circle",
        tooltip = shinyWidgets::tooltipOptions(
          placement = "bottom",
          title = "Select species"
        ),
        fluidRow(
          shinydashboardPlus::box(
            width = 12,
            title = glue::glue("Add species in {wf_label}"),
            status = "primary", 
            # info_dropdown(
            #   inputId = ns("wfinfo"),
            #   placement = "bottom-start",
            #   md_path =  "inst/app/www/info_buttons_docs/wf_features.md"
            # ),
            helpText("Select species from provided list or type to add a new species"),
            selectizeInput(
              inputId = slctize_id,
              label=NULL,
              choices = spp_choices,
              multiple=TRUE,
              selected = if(wf_label == init_wf_label) init_spp_label else NULL, #init_spp_label,
              options = list(
                placeholder = "Select from list or add new species",
              create = TRUE)
            )
          )
        ),
        div(
          actionButton(
            inputId = dpdn_close_id,
            label = "Close"),
          style = "float: right"
        ),
        br()
      ),
      class = "col-inline-btns"
      
      # shinyWidgets::dropMenu(
      #   shinyWidgets::actionBttn(
      #     btn_add_id,
      #     style = "material-circle",
      #     color = "success",
      #     size = "sm",
      #     icon = icon("plus")) %>%
      #     bsplus::bs_embed_tooltip(
      #       title = "Add species",
      #       placement = "bottom"
      #     ),
      #   placement = "bottom-end",
      #   arrow = FALSE,
      #   h5("Select and/or add species in windfarm"),
      #   selectizeInput(
      #     width = "500px",
      #     inputId = slctize_id,
      #     label=NULL,
      #     choices = sort(c("Arctic Skua", "Northern Fulmar",
      #                      "Great Black-backed Gull", "Common Guillemot",
      #                      "Northern Gannet", "Black-legged Kittiwake",
      #                      "Lesser Black-Backed Gull", "Little Auk", "Atlantic Puffin",
      #                      "Razorbill", "Arctic Tern", "Black-headed Gull",
      #                      "Black-throated Diver", "Common Gull", "Common Scoter",
      #                      "Common Tern", "Cormorant", "Eider", "European Shag",
      #                      "Herring Gull", "Little Gull", "Manx Shearwater",
      #                      "Red-throated Diver", "Sandwich Tern")),
      #     multiple=TRUE,
      #     selected = "Black-legged Kittiwake",
      #     options = list(
      #       placeholder = "Select from list or add new species",
      #       create = TRUE)
      #   )
      # ),
      # class = "col-inline-btns"
      
    ),
    col_6(
      shinyWidgets::dropdown(
        inputId = drpdwn_btn_id,
        icon = icon("file-excel"),
        #circle = TRUE,
        right = TRUE,
        size = "sm",
        status = "success",
        width = "600px",
        style = "material-circle",
        tooltip = shinyWidgets::tooltipOptions(placement = "bottom",
                                               title = "Import inputs"),
        p(tags$em("Note: This feature is still under development and currently this panel and its UI elements are simply acting as placeholders."), 
            style = "color: red; font-size: 13px; line-height: 17px"),
        h5(strong("Upload Input Values")),
        helpText("Values of input parameters for multiple species can be 
                 automatically populated via a spreadsheets file"),
        fluidRow(
          col_6(
            p(strong("Template file")),
            downloadButton(outputId = dwnl_btn_id, "Download Template")
          ),
          col_6(
            fileInput(inputId = file_input_id, label = "Upload file")
          )
        )
      ),
      class = "col-inline-btns"
    )
  )
}