#' Filtering operator
#'
#' @description A special %in% operator that doesn't filter nulls (returns TRUE)
#' @param e1 function will check if the values of the first argument are present in the second argument
#' @param e2 function will check if the values of the first argument are present in the second argument
#' @rdname modifiedIn
#' @export

`%modifiedIn%` <- function(e1, e2) {
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
      shiny::uiOutput(ns("study_status_widget")),
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
    # These choices will supply the widgets
    choices <- list(
      source_choices = shiny::reactiveVal(),
      dataset_choices = shiny::reactiveVal(),
      release_daterange_min = shiny::reactiveVal(),
      release_daterange_max = shiny::reactiveVal(),
      status_choices = shiny::reactiveVal(),
      study_status_choices = shiny::reactiveVal()
    )

    # parsing and error handling for choices
    shiny::observe({
      # only display unique choices
      choices$source_choices(unique(manifest()$source))
      choices$dataset_choices(unique(manifest()$dataset_type))
      choices$status_choices(unique(manifest()$status))

      if ("study_status" %in% colnames(manifest())) {
        choices$study_status_choices(unique(manifest()$study_status))
      }

      # if scheduled_release_date is all NA coerce choice to NULL
      # else set min and max so there's some buffer
      is_all_na <- all(is.na(manifest()$scheduled_release_date))

      if (is_all_na) {
        choices$release_daterange_min(NULL)
        choices$release_daterange_max(NULL)
      } else {
        min_date <- min(manifest()$scheduled_release_date, na.rm = T) %m-% months(1)
        max_date <- max(manifest()$scheduled_release_date, na.rm = T) %m+% months(1)

        choices$release_daterange_min(min_date)
        choices$release_daterange_max(max_date)
      }
    })

    # RENDER WIDGETS --------
    output$filter_widgets <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(
          inputId = ns("source_select"),
          label = "Filter by source(s)",
          choices = choices$source_choices(),
          selected = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = ns("dataset_select"),
          label = "Filter by dataset type(s)",
          choices = choices$dataset_choices(),
          selected = NULL,
          multiple = TRUE
        ),
        shiny::dateRangeInput(
          inputId = ns("scheduled_release_daterange"),
          label = "Filter by scheduled release date",
          start = NA,
          end = NA,
          min = choices$release_daterange_min(),
          max = choices$release_daterange_max()
        ),
        shiny::selectInput(
          inputId = ns("status_select"),
          label = "Filter by status",
          choices = choices$status_choices(),
          selected = NULL,
          multiple = TRUE
        )
      )
    })

    # optionally render study_status widget
    output$study_status_widget <- shiny::renderUI({
      # if study_status is present in manifest show checkbox group
      if ("study_status" %in% colnames(manifest())) {
        shiny::selectInput(
          inputId = ns("study_status_select"),
          label = "Filter by study status",
          choices = choices$study_status_choices(),
          selected = NULL,
          multiple = TRUE
        )
      } else {
        NULL
      }
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

    selected_study_status_modified <- shiny::reactive({
      selected_study_status <- input$study_status_select

      if (!is.null(selected_study_status)) {
        selected_study_status[selected_study_status == "NA"] <- NA
      }
      return(selected_study_status)
    })


    # FILTER INPUTS ---------
    # Filters output NULL when nothing is selected. This was filtering out all
    # rows so no data would show in the dashboard. Using %modifiedIn% catches
    # fixes this issue.
    manifest_filtered <- shiny::reactive({
      shiny::req(manifest())

      filtered <- dplyr::filter(
        .data = manifest(),
        source %modifiedIn% input$source_select,
        dataset_type %modifiedIn% selected_data_type_modified(),
        status %modifiedIn% selected_statuses_modified()
      )

      # handle study_status if present in manifest
      if ("study_status" %in% colnames(manifest())) {
        filtered <- filtered %>%
          dplyr::filter(
            study_status %modifiedIn% selected_study_status_modified()
          )
      }

      # Only run when both min and max dateRange has been selected
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
    shiny::observeEvent(input$clear_btn, {
      shinyjs::reset("filter_widgets")
      shinyjs::reset("study_status_widget")
    })

    # RETURN FILTERED MANIFEST ---------
    return(manifest_filtered)
  })
}
