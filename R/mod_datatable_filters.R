#' Filters for dataFlow Manifest UI
#'
#' @description A shiny Module that renders filters for the dataFlow manifest
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @export

mod_datatable_filters_ui <- function(id,
                                     width = NULL,
                                     contributor_choices = c("Contributor 1", "Contributor 2"),
                                     dataset_choices = c("dataset 1", "dataset 2"),
                                     release_daterange = c(Sys.Date(), (Sys.Date() + 365)),
                                     status_choices = c("status 1", "status 2")){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(title = "Filter Datasets",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        width = width,
                        status = "primary",

                        shiny::selectInput(ns("contributor_select"),
                                           label = "Filter by contributor(s)",
                                           choices = contributor_choices,
                                           selected = contributor_choices,
                                           multiple = TRUE),

                        shiny::selectInput(ns("dataset_select"),
                                           label = "Filter by dataset type(s)",
                                           choices = dataset_choices,
                                           selected = dataset_choices,
                                           multiple = TRUE),

                        shiny::dateRangeInput(ns("release_scheduled_daterange"),
                                              label = "Filter by release scheduled date range",
                                              start = release_daterange[1],
                                              end = release_daterange[2]),

                        shiny::checkboxGroupInput(ns("choose_status_checkbox"),
                                                  label = "Filter by status",
                                                  choices = status_choices,
                                                  selected = status_choices)
                        )
  )
}

#' Filters for dataFlow Manifest Server.
#'
#' @param id internal Shiny ID
#' @param manifest a dataFlow manifest
#'
#' @returns a filtered dataframe
#'
#' @export

mod_datatable_filters_server <- function(id,
                                         manifest){

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # CHANGE "NA" TO NA --------
    selected_datasets_modified <- shiny::reactive({
      # replace string "NA" with true NA
      datasets <- input$dataset_select
      datasets[datasets == "NA"] <- NA
      datasets
    })


    # FILTER INPUTS ---------
    manifest_filtered <- shiny::reactive({
      manifest <- manifest()

      # FIXME: For some reason line 75 cause a warning
      # Problem while computing `..3 = ... | is.na(release_scheduled)`. Input `..3` must be of size 19 or 1, not size 0.
      # No error seems to be introduced so I will keep this line of code for now
      filtered <- manifest %>%
        dplyr::filter(contributor %in% input$contributor_select,
                      dataset %in% selected_datasets_modified(),
                      release_scheduled >= input$release_scheduled_daterange[1] & release_scheduled <= input$release_scheduled_daterange[2] | is.na(release_scheduled),
                      data_flow_status %in% input$choose_status_checkbox)


      return(filtered)
    })

    return(manifest_filtered)
  })
}

## To be copied in the UI
# mod_datatable_filters_ui("datatable_filters_1")

## To be copied in the server
# mod_datatable_filters_server("datatable_filters_1")
