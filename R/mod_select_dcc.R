#' Select a DCC UI
#'
#' @description A shiny module. Outputs a selectInput dropdown of Synapse
#' storage project names to the UI.
#'
#' @param id module id
#' @param dcc_config DCC configuration file sourced from
#' `Sage-Bionetworks/data_flow_config`
#'
#' @importFrom shiny NS tagList
#' @export

mod_select_dcc_ui <- function(id,
                              dcc_config) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shinydashboard::box(
      title = "Select a DCC",

      # DCC dropdown
      shiny::selectInput(
        inputId = ns("select_dcc"),
        label = NULL,
        choices = stats::setNames(
          dcc_config$synapse_asset_view,
          dcc_config$project_name
        )
      ),

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
#' @param dcc_config DCC configuration file sourced from
#' `Sage-Bionetworks/data_flow_config`
#' @param access_token Synapse PAT
#'
#' @export

mod_select_dcc_server <- function(id,
                                  dcc_config,
                                  access_token) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # check that inputs are not reactive
    if (shiny::is.reactive(dcc_config)) {
      stop("dcc_config must not be a reactive")
    }
    if (shiny::is.reactive(access_token)) {
      stop("access_token must not be a reactive")
    }

    # put asset views into named list
    all_asset_views <- stats::setNames(
      dcc_config$synapse_asset_view,
      dcc_config$project_name
    )

    # only display asset_views that user has access to
    has_access <- vapply(all_asset_views, function(x) {
      synapse_access(id = x, access = "DOWNLOAD", auth = access_token)
    }, 1)

    asset_views <- all_asset_views[has_access == 1]

    # if there are no asset_views available, stop
    if (length(asset_views) == 0) {
      stop("You do not have DOWNLOAD access to any supported Asset Views.")
    }

    # update selectInput with available dccs
    shiny::updateSelectInput(
      session = session,
      inputId = "select_dcc",
      choices = asset_views
    )

    # if there is only one asset_view available, go straight to dash by
    # updating the selected tabItem
    if (length(asset_views) == 1) {
      shinyjs::click("submit_btn")
    }

    # on button click return:
    # 1) selected dcc configuration
    # 2) the button click
    shiny::eventReactive(input$submit_btn, {
      list(
        selected_dcc_config = dcc_config[dcc_config$synapse_asset_view == input$select_dcc,],
        btn_click = input$submit_btn
      )
    })
  })
}
