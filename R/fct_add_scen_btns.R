#' Action buttons for adding and uploading scenarios (UI function)
#'
#' @description 
#' Inserts buttons controlling the addition of scenarios and input uploading. It
#' should be inserted in the `title` argument of `shinydashboard::tabBox()`
#'   
#' @param btn_add_id character string, the `inputId` of the button controlling the addition of scenarios
#' @param drpdwn_btn_id character string, the `inputId` of the button
#'   controlling the dropdown panel with widgets for uploading input parameter
#'   values
#' @param dwnl_btn_id character string, the `inputId` of the download file widget
#' @param file_input_id character string, the `inputId` of the upload file widget
#'
#' @return No value returned
#' 
add_scn_btns <- function(btn_add_id, 
                         drpdwn_btn_id, 
                         dwnl_btn_id, 
                         file_input_id){
  
  fluidRow(
    col_6(
      shinyWidgets::circleButton(
        inputId =  btn_add_id,
        status = "success",
        size = "sm",
        icon = icon("plus")
      ) %>%
        bsplus::bs_embed_tooltip(
          title = "Add scenario", 
          placement = "bottom"),
      class = "css_col_inline_btns"
    ),
    col_6(
      shinyWidgets::dropdownButton(
        inputId = drpdwn_btn_id,
        icon = icon("file-excel"),
        circle = TRUE,
        right = TRUE,
        size = "sm",
        status = "success",
        width = "600px",
        tooltip = shinyWidgets::tooltipOptions(placement = "bottom",
                                               title = "Import inputs"),
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
      class = "css-col-inline-btns"
    )
  )
}