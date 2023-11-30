#' Filters for dataFlow Manifest UI
#'
#' @description A shiny Module that renders filters for the data flow manifest.
#' @param id shiny id
#' @param width filter width
#' @param contributor_choices vector of choices for contributor filter
#' @param dataset_choices vector of choices for dataset filter
#' @param release_daterange vector containing max/min for release dateRange
#' @param status_choices vector of choices for status filter
#'
#' @importFrom shiny NS tagList
#' @export

mod_datatable_filters_ui <- function(id,
                                     width = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "Filter Datasets",
      collapsible = TRUE,
      collapsed = TRUE,
      width = width,
      status = "primary",
      shiny::uiOutput(ns("filter_widgets"))
    )
  )
}

#' Filters for dataFlow Manifest Server.
#'
#' @param id shiny id
#' @param manifest A data flow manifest.
#'
#' @returns a filtered dataframe
#'
#' @export

mod_datatable_filters_server <- function(id,
                                         manifest) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    choices <- list(
      contributor_choices = reactiveVal(),
      dataset_choices = reactiveVal(),
      release_daterange_start = reactiveVal(),
      release_daterange_end = reactiveVal(),
      status_choices = reactiveVal()
    )

    # GENERATE CHOICES --------
    observe({
      choices$contributor_choices(unique(manifest()$contributor))
      choices$dataset_choices(unique(manifest()$dataset_type))
      choices$release_daterange_start(max(manifest()$scheduled_release_date, na.rm = T))
      choices$release_daterange_end(min(manifest()$scheduled_release_date, na.rm = T))
      choices$status_choices(unique(manifest()$status))
    })

    # RENDER WIDGETS --------

    output$filter_widgets <- shiny::renderUI({
      tagList(
        shiny::selectInput(ns("contributor_select"),
                           label = "Filter by contributor(s)",
                           choices = choices$contributor_choices(),
                           selected = NULL,
                           multiple = TRUE
        ),
        shiny::selectInput(ns("dataset_select"),
                           label = "Filter by dataset type(s)",
                           choices = choices$dataset_choices(),
                           selected = NULL,
                           multiple = TRUE
        ),
        shiny::dateRangeInput(ns("release_scheduled_daterange"),
                              label = "Filter by scheduled release date",
                              start = choices$release_daterange_start()[1],
                              end = choices$release_daterange_end()[2]
        ),
        shiny::checkboxGroupInput(ns("choose_status_checkbox"),
                                  label = "Filter by status",
                                  choices = choices$status_choices(),
                                  selected = NULL
        )
      )
    })


    # CHANGE "NA" TO NA --------
    selected_datasets_modified <- shiny::reactive({
      shiny::req(input$dataset_select)
      # replace string "NA" with true NA
      datasets <- input$dataset_select
      datasets[datasets == "NA"] <- NA
      datasets
    })


    # FILTER INPUTS ---------
    manifest_filtered <- shiny::reactive({
      manifest <- manifest()

      # FIXME: For some reason line 75 cause a warning
      # Problem while computing `..3 = ... | is.na(release_scheduled)`.
      # Input `..3` must be of size 19 or 1, not size 0.
      # No error seems to be introduced so I will keep this line of code for now
      filtered <- manifest %>%
        dplyr::filter(
          contributor %in% input$contributor_select,
          dataset_type %in% selected_datasets_modified(),
          scheduled_release_date >=
            input$release_scheduled_daterange[1] &
            scheduled_release_date <=
              input$release_scheduled_daterange[2] |
            is.na(scheduled_release_date),
          status %in% input$choose_status_checkbox
        )


      return(filtered)
    })

    return(manifest_filtered)
  })
}
