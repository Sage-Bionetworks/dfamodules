#' Tabbed dashboard module UI (deprecated)
#'
#' @description A shiny module that displays a tabbed dashboard
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @export

mod_tabbed_dashboard_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(

    # tabbox
    shinydashboard::tabBox(
      title = "Dataset Dashboard",
      width = NULL,
      side = "right",

      # show different views of dataset on different tab panels

      # all unreleased data
      shiny::tabPanel("Unreleased",
                      mod_datatable_dashboard_ui(ns("datatable_dashboard_unreleased"))),

      # unreleased, no embargo, passing all checks
      # aka ready for release
      shiny::tabPanel("Ready for release",
                      mod_datatable_dashboard_ui(ns("datatable_dashboard_ready"))),

      # released_scheduled = NA
      shiny::tabPanel("Not scheduled",
                      mod_datatable_dashboard_ui(ns("datatable_dashboard_not_scheduled"))),

      # all
      shiny::tabPanel("All",
                      mod_datatable_dashboard_ui(ns("datatable_dashboard_all"))),

      # released = TRUE
      shiny::tabPanel("Previously released",
                      mod_datatable_dashboard_ui(ns("datatable_dashboard_archive")))
    )
  )
}

#' Tabbed dashboard module UI (deprecated)
#' @param id tabbed dashboard ID
#' @param df dataframe with certain columns
#' @param config datatable dashboard config
#' @export

mod_tabbed_dashboard_server <- function(id, df, config){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # TODO: Remove hard coded column names
    # I think column names will come from data flow component

    # subset dataframe into various views
    unreleased_datasets <- shiny::reactive({
      data <- df()
      data[ data$released == FALSE, ]
    })

    # not scheduled
    not_scheduled_datasets <- shiny::reactive({
      data <- df()
      data[ is.na(data$release_scheduled), ]
    })

    # all checks passing / no embargo / unreleased (i.e. ready for release)
    all_checks_passed_datasets <- shiny::reactive({

      qc_cols <- "standard_compliance"

      data <- df()

      # which rows have all qc_cols == TRUE
      passing <- apply(data[qc_cols], 1, all)

      # which rows have passed their embargo date or are NA
      no_embargo <- data$embargo <= Sys.Date() | is.na(data$embargo)

      # which rows are unreleased
      unreleased <- data$released == FALSE

      # which rows are passing qc, past/have no embargo, and are unreleased
      ready <- passing & no_embargo


      # subset
      data[ ready, ]
    })

    # previously released
    released_datasets <- shiny::reactive({
      data <- df()
      data[ data$released == TRUE, ]
    })

    # render datatables

    mod_datatable_dashboard_server("datatable_dashboard_all",
                                   df,
                                   config)

    mod_datatable_dashboard_server("datatable_dashboard_unreleased",
                                   unreleased_datasets,
                                   config)

    mod_datatable_dashboard_server("datatable_dashboard_not_scheduled",
                                   not_scheduled_datasets,
                                   config)

    mod_datatable_dashboard_server("datatable_dashboard_ready",
                                   all_checks_passed_datasets,
                                   config)

    mod_datatable_dashboard_server("datatable_dashboard_archive",
                                   released_datasets,
                                   config)
  })
}

## To be copied in the UI
# mod_tabbed_dashboard_ui("tabbed_dashboard_1")

## To be copied in the server
# mod_tabbed_dashboard_server("tabbed_dashboard_1")
