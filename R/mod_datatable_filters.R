#' Filtering operator
#'
#' @description A special operator that doesn't filter nulls
#' @rdname modifiedIn
#' @export

`%modifiedIn%` <- function (e1, e2) {
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
      shiny::uiOutput(ns("filter_widgets")),
      shiny::actionButton(ns("clear_btn"), "Clear Filter Selections")
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
#' @importFrom lubridate %m+%
#' @importFrom lubridate %m-%
#' @export

mod_datatable_filters_server <- function(id,
                                         manifest) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # GENERATE CHOICES --------
    choices <- list(
      contributor_choices = reactiveVal(),
      dataset_choices = reactiveVal(),
      release_daterange_min = reactiveVal(),
      release_daterange_max = reactiveVal(),
      status_choices = reactiveVal()
    )

    observe({
      choices$contributor_choices(unique(manifest()$contributor))
      choices$dataset_choices(unique(manifest()$dataset_type))
      choices$status_choices(unique(manifest()$status))

      is_all_na <- all(is.na(manifest()$scheduled_release_date))

      if (is_all_na) {
        choices$release_daterange_min(NULL)
        choices$release_daterange_max(NULL)
        print("ALL NA")
      } else {

        min_date <- min(manifest()$scheduled_release_date, na.rm = T) %m-% months(1)
        max_date <- max(manifest()$scheduled_release_date, na.rm = T) %m+% months(1)

        choices$release_daterange_min(min_date)
        choices$release_daterange_max(max_date)
      }
    })

    # RENDER WIDGETS --------
    output$filter_widgets <- shiny::renderUI({

      print(paste0("min date - ", choices$release_daterange_min()))
      print(paste0("max date - ", choices$release_daterange_max()))

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
                              end = NA,
                              min = choices$release_daterange_min(),
                              max = choices$release_daterange_max()
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
          contributor %modifiedIn% input$contributor_select,
          dataset_type %modifiedIn% selected_data_type_modified(),
          status %modifiedIn% selected_statuses_modified()
        )

      if (all(!is.na(input$scheduled_release_daterange)) &
          all(!is.null(input$scheduled_release_daterange))) {
        filtered <- filtered %>%
          dplyr::filter(
            scheduled_release_date >= input$scheduled_release_daterange[1] &
              scheduled_release_date <= input$scheduled_release_daterange[2]
          )
      }

      return(filtered)
    })

    # CLEAR SELECTIONS ---------
    observeEvent(input$clear_btn, { shinyjs::reset("filter_widgets") })

    # RETURN FILTERED MANIFEST ---------
    return(manifest_filtered)
  })
}
