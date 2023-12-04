#' Filtering operator
#'
#' @description A special operator that doesn't filter nulls
#' @rdname special_pipe
#' @export

`%==%` <- function (e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return(e1 %in% e2)
  }
}

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
    shinyjs::useShinyjs(),
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

    # GENERATE CHOICES --------
    choices <- list(
      contributor_choices = reactiveVal(),
      dataset_choices = reactiveVal(),
      release_daterange_start = reactiveVal(),
      release_daterange_end = reactiveVal(),
      status_choices = reactiveVal()
    )

    observe({
      choices$contributor_choices(unique(manifest()$contributor))
      choices$dataset_choices(unique(manifest()$dataset_type))
      choices$status_choices(unique(manifest()$status))

      is_all_na <- all(is.na(manifest()$scheduled_release_date))
      if (is_all_na) {
        choices$release_daterange_start(NA)
        choices$release_daterange_end(NA)
      } else {
        choices$release_daterange_start(max(manifest()$scheduled_release_date, na.rm = T))
        choices$release_daterange_end(min(manifest()$scheduled_release_date, na.rm = T))
      }
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
        shiny::dateRangeInput(ns("scheduled_release_daterange"),
                              label = "Filter by scheduled release date",
                              start = NA,
                              end = NA
        ),
        shiny::selectInput(ns("status_select"),
                           label = "Filter by status",
                           choices = choices$status_choices(),
                           selected = NULL,
                           multiple = TRUE
        )
      )
    })

    # HANDLE NA ---------
    # for some reason shiny::SelectInput converts NA to "NA"
    # This logic helps everything filter nicely
    selected_data_type_modified <- shiny::reactive({
      selected_datasets <- input$dataset_select

      if (!is.null(selected_datasets)) {
        selected_datasets[selected_datasets == "NA"] <- NA
      }
      return(selected_datasets)
    })

    selected_statuses_modified <- shiny::reactive({
      selected_status <- input$status_select

      if (!is.null(selected_status)) {
        selected_status[selected_status == "NA"] <- NA
      }
      return(selected_status)
    })

    # FILTER INPUTS ---------
    manifest_filtered <- shiny::reactive({
      req(manifest())

      filtered <- manifest() %>%
        dplyr::filter(
          contributor %==% input$contributor_select,
          dataset_type %==% selected_data_type_modified(),
          status %==% selected_statuses_modified()
        )

      if (all(!is.na(input$scheduled_release_daterange)) & all(!is.null(input$scheduled_release_daterange))) {
        filtered <- filtered %>%
          dplyr::filter(
            scheduled_release_date >= input$scheduled_release_daterange[1] &
              scheduled_release_date <= input$scheduled_release_daterange[2]
          )
      }

      return(filtered)
    })

    return(manifest_filtered)
  })
}
