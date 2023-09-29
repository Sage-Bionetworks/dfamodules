#' Select a DCC UI
#'
#' @description A shiny module. Outputs a selectInput dropdown of Synapse storage project names to the UI.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @export

mod_select_dcc_ui <- function(id,
                              dcc_config) {
  ns <- shiny::NS(id)
  shiny::tagList(

    # DCC dropdown
    selectInput(ns("select_dcc"),
      "Select a DCC",
      choices = setNames(
        dcc_config$synapse_asset_view,
        dcc_config$project_name
      )
    ),

    # Button to initiate selection
    shiny::actionButton(
      ns("submit_btn"),
      "Select Project"
    ),
  )
}

# Select a DCC Module Server
#'
#' @export

mod_select_dcc_server <- function(id,
                                  dcc_config,
                                  access_token) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # check that inputs are not reactive
    if (is.reactive(dcc_config)) {
      stop("dcc_config must not be a reactive")
    }
    if (is.reactive(access_token)) {
      stop("access_token must not be a reactive")
    }

    # put asset views into named list
    all_asset_views <- setNames(
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
    updateSelectInput(
      session = session,
      inputId = "select_dcc",
      choices = asset_views
    )

    # on button click return:
    # 1) selected dcc
    # 2) the button click
    eventReactive(input$submit_btn, {
      list(
        selected_dcc = asset_views[asset_views == input$select_dcc],
        btn_click = input$submit_btn
      )
    })
  })
}
