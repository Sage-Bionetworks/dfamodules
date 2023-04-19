# Storage Project Selection Module UI

#' @title select_storage_project_ui and select_storage_project_server
#' @description A shiny module. Outputs a selectInput dropdown of Synapse storage project names to the UI.
#' @return To the server: A list information from the module. `selected_df` - a dataframe with a single row containing the `name` and `id` of the selected storage project. `action_btn` - TRUE/FALSE output from button click.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_storage_project_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Select Project",
      width = NULL,
      
      # Project dropdown
      uiOutput(ns("project_selector")),
      
      # Button to initiate project selection
      actionButton(ns("submit_btn"),
                   "Select Project"),
      )
    )
}
    
# Storage Project Selection Module Server
#'
#' @noRd 
mod_select_storage_project_server <- function(id, asset_view, input_token, base_url) {
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # API CALL : GET STORAGE PROJECTS #######################################################################

    storage_project_obj <- storage_projects(asset_view = asset_view,
                                            input_token = input_token,
                                            base_url = base_url)
    
    # DROP DOWN LISTING STORAGE PROJECTS ####################################################################
    
    # render ui for storage project drop down
    output$project_selector <- shiny::renderUI({
      
       selectInput(inputId = ns("selected_project"),
                   label = NULL,
                   choices = storage_project_obj$content[,"name"],
                   selectize = FALSE) # must be false or for some reason cannot reference `input$selected_project`
      
    })
    
    # SUBSET STORAGE PROJECT DATAFRAME BY SELECTED PROJECT  ##############################################################
    
    selected_project_df <- reactive({
      
      req(input$selected_project)
      
      storage_project_obj$content[ match(input$selected_project, storage_project_obj$content[,"name"]), ]
      })
    
    # RETURN SELECTED PROJECT  ##############################################################
    
    eventReactive(input$submit_btn, {
      return(selected_project_df())
    })
  })
  }
    
## To be copied in the UI
# mod_select_storage_project_ui("select_storage_project_1")
    
## To be copied in the server
# mod_select_storage_project_server("select_storage_project_1")