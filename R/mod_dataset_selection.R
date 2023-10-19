#' Select a Dataset UI Function
#'
#' @description A Shiny module that displays the datasets in a given asset view in a selectable DT::datatable
#'
#' @param id shiny id
#' @importFrom shiny NS tagList
#' @export

mod_dataset_selection_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(

    ## SELECT DATASET BOX  ####################################################
    shiny::fluidRow(
      waiter::useWaiter(),
      shiny::column(
        width = 12,
        shiny::div(
          id = ns("select_dataset_wrapper"),
          shinydashboard::box(
            title = "Select Dataset",
            width = NULL,

            # Table of storage project datasets
            DT::DTOutput(ns("dataset_tbl")),
            shiny::br(),

            # Button to initiate dataset selection
            shiny::actionButton(ns("submit_btn"), "Select Dataset(s)"),
            shiny::br()
          )
        )
      )
    )
  )
}

#' Select a Dataset Server Function
#'
#' @param id shiny id
#' @param storage_project_df Dataframe containing the columns `id` and `name`
#' @param asset_view Fileview containing datasets
#' @param access_token Synapse PAT
#' @param hidden_datasets vector of synIds to hide
#' @param base_url Schematic REST API base url
#'
#' @export

mod_dataset_selection_server <- function(id,
                                         storage_project_id,
                                         asset_view,
                                         access_token,
                                         hidden_datasets = NULL,
                                         base_url) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # GET DATASETS FOR SELECTED STORAGE PROJECT  ###############################
    datasets <- shiny::reactive({
      req(storage_project_id())

      # show waiter
      waiter::waiter_show(
        id = ns("select_dataset_wrapper"),
        html = htmltools::div(
          style = "color:#424874;",
          waiter::spin_3(),
          htmltools::h4("Retrieving datasets...")
        )
      )

      # on exit - hide waiter
      on.exit(waiter::waiter_hide())

      # call schematic API storage/project/datasets
      storage_project_datasets_obj <- storage_project_datasets(
        asset_view = asset_view(),
        project_id = storage_project_id(),
        access_token = access_token,
        base_url = base_url
      )

      # return content
      return(storage_project_datasets_obj$content)
    })

    output$tst <- renderPrint({
      selected_datasets()
    })

    #  RENDER TABLE ###########################################################
    output$dataset_tbl <- DT::renderDataTable({
      # validate returned object
      # if there are no datasets returned, show message
      req(datasets())
      shiny::validate(
        shiny::need(
          nrow(datasets()) > 0, "No datasets available"
        )
      )

      # data table with scroll bar, no pagination, and filtering
      DT::datatable(datasets(),
        selection = "multiple",
        option = list(
          scrollY = 500,
          scrollCollapse = TRUE,
          bPaginate = FALSE,
          dom = "t"
        ),
        filter = list(position = "top", clear = TRUE)
      )
    })

    # CREATE RETURNED DATAFRAME OF SELECTED DATASETS  ##########################
    selected_datasets <- shiny::reactive({
      # get selected rows from datatable
      selected <- input$dataset_tbl_rows_selected

      # subset
      return(datasets()[selected, ])
    })

    # RETURN DATA ON CLICK  ####################################################
    shiny::eventReactive(input$submit_btn, {
      return(selected_datasets())
    })
  })
}
