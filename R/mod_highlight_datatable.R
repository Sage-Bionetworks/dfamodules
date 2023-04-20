#' highlight_datatable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export

mod_highlight_datatable_ui <- function(id){
  ns <- NS(id)
  tagList(

    DT::DTOutput(ns("highlight_tbl")),
  )
}

#' highlight_datatable Server Functions
#'
#' @noRd
#' @export

mod_highlight_datatable_server <- function(id,
                                           df,
                                           selection,
                                           df_match_colname){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create selected TRUE/FALSE idx
    selection_idx <- reactive({
      df()[[df_match_colname]] %in% selection()
    })

    # add selection_idx to datatable
    manifest_w_idx <- reactive({
      selection_idx_df <- data.frame(selection_idx = selection_idx())
      dplyr::bind_cols(df(), selection_idx_df)
    })

    hide_idx <- reactive({
      # hide selection index column
      hide_idx <- match("selection_idx", names(manifest_w_idx()))
    })

    output$highlight_tbl <- DT::renderDataTable({

      # datatable with x and y scrollbar, no pagination
      # hiding the selection index column
      dt <- DT::datatable(manifest_w_idx(),
                          escape = FALSE,
                          selection = "none",
                          filter = "top",
                          options = list(scrollX = TRUE,
                                         scrollY = 800,
                                         bPaginate = FALSE,
                                         columnDefs = list(list(targets = hide_idx(),
                                                                visible = FALSE))))

      # add conditional styling based on hidden column
      dt <- DT::formatStyle(table = dt,
                            "selection_idx", target = "row",
                            backgroundColor = DT::styleEqual(TRUE, "#ffe2ad"))

      dt
      })
  })
}

## To be copied in the UI
# mod_highlight_datatable_ui("highlight_datatable_1")

## To be copied in the server
# mod_highlight_datatable_server("highlight_datatable_1")
