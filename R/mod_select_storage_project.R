#' Storage Project Selection Module UI
#'
#' @description A shiny module. Outputs a selectInput dropdown of Synapse
#' storage project names to the UI.
#'
#' @param id shiny id
#'
#' @importFrom shiny NS tagList
#' @export

mod_select_storage_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "Select Project",
      width = NULL,

      # Project dropdown
      shiny::selectInput(
        inputId = ns("selected_project"),
        label = NULL,
        choices = "downloading data...",
        selectize = FALSE), # must be false or for some reason cannot reference `input$selected_project`

      # Button to initiate project selection
      shiny::actionButton(
        ns("submit_btn"),
        "Select Project"
      ),
    )
  )
}

#' Storage Project Selection Module Server
#' @param id shiny id
#' @param asset_view An ID of a file view listing all project data assets
#' @param access_token A Synapse Personal Access Token
#' @param base_url A Schematic API base URL. Default is
#' `https://schematic.api.sagebionetworks.org`
#'
#' @export

mod_select_storage_project_server <- function(id,
                                              asset_view,
                                              access_token,
                                              base_url = paste0(
                                                "https://",
                                                "schematic.api.sagebionetworks",
                                                ".org"
                                              )) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # API CALL : GET STORAGE PROJECTS #######################################################################
    # UPDATE STORAGE PROJECT DROP DOWN W API RES ##############################
    shiny::observe({
      shiny::req(asset_view())

      sp_obj <- storage_projects(
        asset_view = asset_view(),
        access_token = access_token,
        base_url = base_url)

      choices <- stats::setNames(
        sp_obj$content$id,
        sp_obj$content$name
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "selected_project",
        choices = choices
      )
      })

    # # RETURN SELECTED PROJECT  ##############################################################

    shiny::eventReactive(input$submit_btn, {
      return(input$selected_project)
    })
  })
}
