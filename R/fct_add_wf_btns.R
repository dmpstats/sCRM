#' Action buttons for adding and uploading Windfarms (UI function)
#'
#' @description 
#' Inserts buttons controlling the addition of Windfarms and input uploading. It
#' should be inserted in the `title` argument of `shinydashboard::tabBox()`
#'   
#' @param drpdwn_add_wf character string, the `inputId` of the dropdown menu with wf addition widget
#' @param drpdwn_btn_id character string, the `inputId` of the button
#'   controlling the dropdown panel with widgets for uploading input parameter
#'   values
#' @param dwnl_btn_id character string, the `inputId` of the download file widget
#' @param file_input_id character string, the `inputId` of the upload file widget
#'
#' @return No value returned
#' 
#' @noRd 
add_wf_btns <- function(dpdn_add_wf_id,
                        dpdn_close_id,
                        sltz_wf_id,
                        dpdn_upld_wf_id, 
                        dwnl_btn_id, 
                        file_input_id){
  
  # Buttons on tab title
  fluidRow(
    col_6(
      shinyWidgets::dropdown(
        inputId = dpdn_add_wf_id,
        icon = icon("plus"),
        #circle = TRUE,
        right = TRUE,
        size = "sm",
        status = "success",
        width = "400px",
        style = "material-circle",
        tooltip = shinyWidgets::tooltipOptions(
          placement = "bottom",
          title = "Add Windfarm"),
        fluidRow(
          shinydashboardPlus::box(
            title = "Add windfarm(s)",
            status = "primary", 
            width = 12, 
            helpText("To add a windfarm, type-in a label and press Return"),
            selectizeInput(
              inputId = sltz_wf_id,
              label = NULL,
              choices = init_wf_label,
              multiple = TRUE, 
              selected = init_wf_label,
              options = list(
                placeholder = "Provide Windfarm name(s)",
                create = TRUE,
                persist = FALSE)
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
        # span(
        #   shinyWidgets::actionBttn(
        #     inputId = "drpdwn-close", 
        #     style = "bordered", 
        #     label = "Close", 
        #     color = "primary", 
        #     size = "xs"),
        #   style="float:right"
        # )
        # col_10(
        #   textInput(
        #     inputId = "wf-label", 
        #     label = h5(strong("Wind farm Name")), 
        #     value = "",
        #     placeholder = "Provide unique name")
        # ),
        # col_2(
        #   style = "margin-top: 36px; margin-left: -10px",
        #   # actionButton(
        #   #   inputId = btn_add_id, 
        #   #   label = "Add",
        #   #   style = "background-color: #28b78d"
        #   # )
        #   shinyWidgets::actionBttn(
        #     inputId = btn_add_id,
        #     style = "simple",
        #     label = "Add", 
        #     size = "sm",
        #     color = "success"
        #   )
        # ),
        # br(),

        
      ),
      # shinyWidgets::actionBttn(
      #   inputId =  btn_add_id,
      #   style = "material-circle",
      #   color = "success",
      #   size = "sm",
      #   icon = icon("plus")
      # ) %>%
      #   bsplus::bs_embed_tooltip(
      #     title = "Add scenario", 
      #     placement = "bottom"),
      class = "css-col-inline-btns"
    ),
    col_6(
      shinyWidgets::dropdown(
        inputId = dpdn_upld_wf_id,
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
        helpText("Values of input parameters for multiple scenarios can be 
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