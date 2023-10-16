#' Datatable Dashboard UI Function
#'
#' @description A shiny module that displays a DataFlow manifest as a dashboard
#'
#' @param id shiny id
#' @importFrom shiny NS tagList
#' @export

mod_datatable_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("datatable_out"))
  )
}

#' datatable_dashboard Server Functions
#'
#' @param id shiny id
#' @param df dataframe containing data to be displayed in dashboard
#' @param config config in `inst/datatable_dashboard_config.json`
#'
#' @export

mod_datatable_dashboard_server <- function(id, df, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # check that inputs are not reactive
    if (!shiny::is.reactive(df)) {
      stop("df must be a reactive")
    }
    if (!shiny::is.reactive(config)) {
      stop("config must be a reactive")
    }

    # render datatable

    output$datatable_out <- DT::renderDataTable({
      create_dashboard(
        df(),
        config()
      )
    })
  })
}
