#' Select a DCC UI
#'
#' @description A shiny module. Outputs a selectInput dropdown of Synapse
#' storage project names to the UI.
#'
#' @param id Module id
#' @param title Title of box element
#'
#' @importFrom shiny NS tagList
#' @export

mod_select_dcc_ui <- function(id,
                              title = "Select a DCC") {
  ns <- shiny::NS(id)
  shiny::tagList(

    shinyjs::useShinyjs(),

    shinydashboard::box(
      title = title,

      # DCC dropdown
      shiny::uiOutput(ns("dcc_dropdown")),

      # Button to initiate selection
      shiny::actionButton(
        inputId = ns("submit_btn"),
        label = "Select Project"
      )
    )
  )
}

#' Select a DCC Module Server
#'
#' @param id module ID
#' @param tenants_config_path DCC configuration file sourced from
#' `Sage-Bionetworks/data_flow_config`
#' @param access_token Synapse PAT
#'
#' @export

mod_select_dcc_server <- function(id,
                                  tenants_config_path,
                                  access_token) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # check that inputs are not reactive
    if (shiny::is.reactive(tenants_config_path)) {
      stop("tenants_config_path must not be a reactive")
    }
    if (shiny::is.reactive(access_token)) {
      stop("access_token must not be a reactive")
    }

    # get tenants config
    tenants_config <- jsonlite::read_json(
      path = tenants_config_path,
      simplifyVector = TRUE
    )

    tenants_config <- tenants_config$tenants

    # put asset views into named vector
    all_asset_views <- stats::setNames(
      tenants_config$synapse_asset_view,
      tenants_config$name
    )

    # determine which asset views user has access to
    has_access <- vapply(all_asset_views, function(x) {
      synapse_access(id = x, access = "DOWNLOAD", auth = access_token)
    }, 1)

    visible_tenants <- tenants_config[has_access == 1,]

    observe({

      # if there are no asset_views available, stop
      if (nrow(visible_tenants) == 0) {

        stop("You do not have DOWNLOAD access to any supported Asset Views.")

      } else if (nrow(visible_tenants) == 1) {

        # if only 1 tenant skip drop down selection
        shinyjs::click("submit_btn")

      } else {

        # populate dropdown with available asset views
        output$dcc_dropdown <- renderUI({
          shiny::selectInput(
            inputId = ns("selected_dcc"),
            label = NULL,
            choices = stats::setNames(
              visible_tenants$synapse_asset_view,
              visible_tenants$name
            )
          )
        })
      }
    })

    # on button click:
    # get dcc specific configuration
    # return configuration + button click output
    shiny::eventReactive(input$submit_btn, {

      # get selected tenant info
      if (nrow(visible_tenants) == 1) {
        tenant <- visible_tenants
      } else {
        tenant <- visible_tenants[visible_tenants$synapse_asset_view == input$selected_dcc,]
      }

      # get tenant dfa config
      config <- jsonlite::read_json(
        path = file.path(
          dirname(tenants_config_path),
          tenant$config_location
        ),
      )

      # return a list with config and btn click output
      list(
        selected_dcc_config = config,
        btn_click = input$submit_btn
      )
    })
  })
}
