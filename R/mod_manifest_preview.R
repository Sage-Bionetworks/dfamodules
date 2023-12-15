#' Manifest Preview UI
#'
#' @description Shows preview of manifest that highlights based on a given input
#'
#' @param id shiny id
#'
#' @importFrom shiny NS tagList
#' @export

mod_manifest_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("highlight_tbl")),
  )
}

#'  Manifest Preview Server
#' @description
#' Shows preview of manifest that highlights based on a given input
#'
#' @param id shiny id
#' @param df a dataframe
#' @param selection a vector of IDs to highlight
#' @param df_match_colname column name to match highlight on
#'
#'
#' @export

mod_manifest_preview_server <- function(id,
                                        df,
                                        selection,
                                        df_match_colname) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # create selected TRUE/FALSE idx
    selection_idx <- shiny::reactive({
      df()[[df_match_colname]] %in% selection()
    })

    # add selection_idx to datatable
    manifest_w_idx <- shiny::reactive({
      selection_idx_df <- data.frame(selection_idx = selection_idx())
      dplyr::bind_cols(df(), selection_idx_df)
    })

    hide_idx <- shiny::reactive({
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
        options = list(
          scrollX = TRUE,
          scrollY = 800,
          bPaginate = FALSE,
          columnDefs = list(
            list(
              targets = hide_idx(),
              visible = FALSE
          ))
        )
      )

      # add conditional styling based on hidden column
      dt <- DT::formatStyle(
        table = dt,
        "selection_idx", target = "row",
        backgroundColor = DT::styleEqual(TRUE, "#ffe2ad")
      )

      dt
    })
  })
}
