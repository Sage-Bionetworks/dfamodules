#' dashboard_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export


mod_dashboard_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("filters")),
    actionButton(ns("apply_filter_btn"), "Apply filters"),
    actionButton(ns("clear_filter_btn"), "Clear filters")
  )
}

#' dashboard_filters Server Functions
#'
#' @export

mod_dashboard_filters_server <- function(id, dashboard_config, manifest) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    # store manifest in reactiveVal
    # this reactiveVal is modified when apply_filter_btn or clear_filter_btn
    # are clicked
    filtered_manifest <- reactiveVal(manifest)

    # pull out filter configuration details
    is_filter <- unlist(purrr::map(dashboard_config, "create_filter"))

    # pull out filtered attribute names
    filter_attributes <- names(is_filter)[is_filter]

    ## PARSE MANIFEST ##########################################################

    # for each filter attribute get the choices / max / min information for widget
    filter_config <- lapply(filter_attributes, function(a) {

      # set to NULL
      max <- NULL
      min <- NULL
      choices <- NULL

      # get single attribute config
      attribute_config <- dashboard_config[names(dashboard_config) %in% a][[a]]

      # if attribute is a date, get max and min
      if (attribute_config$type == "date" | attribute_config$type == "int") {
        min <- min(manifest[[a]], na.rm = TRUE)
        max <- max(manifest[[a]], na.rm = TRUE)

        # if attribute is a string, get unique entries
      } else if (attribute_config$type == "str") {
        choices <- unique(manifest[[a]])
      }

      return(list(min = min,
                  max = max,
                  choices = choices,
                  type = attribute_config$type))

    })

    names(filter_config) <- filter_attributes

    ## RENDER WIDGETS ##########################################################
    output$filters <- renderUI({

      lapply(filter_attributes, function(a) {

        # parse dashboard config
        widget_type <- dashboard_config[[a]]$type

        label <- dashboard_config[[a]]$display_name
        id <- paste0(dashboard_config[[a]]$Attribute, "_filter")

        # for strings create selectInput widget
        if (widget_type == "str") {
          shiny::selectInput(ns(id),
                             label = label,
                             choices = filter_config[[a]]$choices,
                             selected = filter_config[[a]]$choices,
                             multiple = TRUE)

          # for dates create dateRangeInput widgets
        } else if (widget_type == "date") {
          dateRangeInput(ns(id),
                         label = label,
                         start = filter_config[[a]]$min,
                         end = filter_config[[a]]$max)

          # for integers create sliderInput
        } else if (widget_type == "int") {
          sliderInput(ns(id),
                      label = label,
                      min = as.numeric(filter_config[[a]]$min),
                      max = as.numeric(filter_config[[a]]$max),
                      value = c(filter_config[[a]]$min, filter_config[[a]]$max),
                      width = "75%")

          # for booleans create radio button input
        } else if (widget_type == "boolean" | widget_type == "icon") {
          radioButtons(ns(id),
                       label = label,
                       choices = c("TRUE", "FALSE"),
                       selected = "FALSE",
                       width = "75%")
        } else {
          stop("Data type ", widget_type, " not supported at this time")
        }
      })
    })

    ## COMPILE SHINY WIDGET INPUT IN A LIST  ###################################
    widget_inputs <- reactive({

      # for each filter, return input values
      widget_output_list <- lapply(filter_attributes, function(a) {

        filter_id <- paste0(a, "_filter")
        return(input[[filter_id]])
      })

      # name list of input values with filter_attribute
      names(widget_output_list) <- filter_attributes

      return(widget_output_list)
    })

    ## SUBSET MANIFEST  ########################################################
    # on click of apply_filter_btn - subset manifest

    observeEvent(input$apply_filter_btn, {
      # for each filter attribute
      # output T/F vector indicating which rows meet filter criteria
      filter_list <- lapply(1:length(filter_attributes), function(i) {

        # get attribute
        attribute <- filter_attributes[i]

        # get inputs
        inputs <- widget_inputs()[[attribute]]

        # for string attributes find all rows that match selections in selectInput
        if (filter_config[[i]]$type == "str") {

          # replace "NA" string with NA
          inputs[inputs == "NA"] <- NA

          manifest[[attribute]] %in% inputs

          # for date / integer attributes find all rows that fall within dateRange/sliderInput selections
        } else if (filter_config[[i]]$type == "date" | filter_config[[i]]$type == "int" ) {

          manifest[[attribute]] >= min(inputs) & manifest[[attribute]] <= max(inputs)

          # for boolean attributes find all rows that match T/F selection
        } else if (filter_config[[i]]$type == "boolean" | filter_config[[i]]$type == "icon" ) {

          if (inputs == "null") {
            return(NULL)

          } else {
            # make input a boolean
            inputs <- as.logical(inputs)

            grepl(inputs, manifest[[attribute]])
          }
        }
      })

      # add names to list (for use with bind_cols)
      names(filter_list) <- filter_attributes

      # bind into a table where each row represents a row in the manifest
      # and each col represents a filter attribute
      filter_tbl <- dplyr::bind_cols(filter_list)

      # sum of rows (TRUE = 1)
      # keep rows where all attributes are TRUE
      keep_rows <- rowSums(filter_tbl, na.rm = TRUE) == ncol(filter_tbl)

      # # subset manifest
      filtered_manifest(manifest[keep_rows, ])
    })

    ## CLEAR FILTERS
    # on click of clear_filter_btn - clear filters and display full manifest

    observeEvent(input$clear_filter_btn, {

      lapply(filter_attributes, function(a) {

        # parse dashboard config
        widget_type <- dashboard_config[[a]]$type

        label <- dashboard_config[[a]]$display_name
        id <- paste0(dashboard_config[[a]]$Attribute, "_filter")

        # for strings create selectInput widget
        if (widget_type == "str") {
          shiny::updateSelectInput(session = session,
                                   inputId = id,
                                   selected = filter_config[[a]]$choices)

          # for dates create dateRangeInput widgets
        } else if (widget_type == "date") {
          shiny::updateDateRangeInput(session = session,
                                      inputId = id,
                                      start = filter_config[[a]]$min,
                                      end = filter_config[[a]]$max)

          # for integers create sliderInput
        } else if (widget_type == "int") {
          shiny::updateSliderInput(session = session,
                                   inputId = id,
                                   value = c(filter_config[[a]]$min, filter_config[[a]]$max))

          # for booleans create radio button input
        } else if (widget_type == "boolean" | widget_type == "icon") {
          shiny::updateRadioButtons(session = session,
                                    inputId = id,
                                    selected = "FALSE")
        }

        # Revert dashboard manifest to original / unfiltered
        filtered_manifest(manifest)
      })
    })

    return(reactive({filtered_manifest()}))

  })
}


## To be copied in the UI
# mod_dashboard_filters_ui("dashboard_filters_1")

## To be copied in the server
# mod_dashboard_filters_server("dashboard_filters_1")
