#' Select a File UI function
#'
#' @description A Shiny module that displays the files in a given dataset in a selectable DT::datatable
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @export

mod_file_selection_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::fluidRow(
      waiter::use_waiter(),
      shiny::column(width = 12,
                    # Action button that opens file selector
                    # TODO: eventually this will become a toggle similar to data_curator dashboard
                    shiny::div(
                      id = ns("select_files_wrapper"),
                      shinydashboard::box(
                        width = NULL,
                        title = "Select Files",

                        shiny::actionButton(ns("getfiles_btn"), "Get Files"),

                        DT::DTOutput(ns("manifest_tbl"))
                      )
                    )
      )
    )
  )
}

#' Select a File Server Functions
#'
#' @param id internal shiny ID
#' @param dataset dataset folder synId
#' @param asset_view fileview synId
#' @param input_token Syanpse PAT
#'
#' @export

mod_file_selection_server <- function(id, dataset, asset_view, input_token) {

  shiny::moduleServer( id, function(input, output, session) {

    ns <- session$ns

    w <- Waiter$new(id = ns("select_files_wrapper"),
                    html = htmltools::div(
                      style="color:#424874;",
                      waiter::spin_3(),
                      htmltools::h4("Retrieving manifest...")),
                    color = waiter::transparent(.8))


    # ON CLICK GET MANIFEST FOR SELECTED DATASET ############################################################
    # FIXME: Button click data flow is not ideal
    # Click of show file level view must happen AFTER dataset is selected
    # If you have clicked show file level view and displayed a dataset, but want to
    # change that dataset you need to click "Show file level view" again

    manifest <- shiny::eventReactive(input$getfiles_btn, {

      # show waiter
      w$show()

      # on exit - hide waiter
      on.exit({
        w$hide()
      })

      ds <- dataset()
      manifest_download_to_df(asset_view = asset_view,
                              dataset_id = ds$id,
                              input_token = input_token)

    })

    # DISPLAY MANIFEST AS TABLE #############################################################################

    output$manifest_tbl <- DT::renderDataTable({
      DT::datatable(manifest(),
                    option = list(scrollY = 500,
                                  scrollX = TRUE,
                                  scrollCollapse = TRUE,
                                  bPaginate = FALSE,
                                  dom = "t"),
                    filter = list(position = 'top', clear = TRUE))
    })

    # return a list containing the downloaded manifest and rows selected

    return(list(
      manifest = shiny::reactive({ manifest() }),
      selected_rows = shiny::reactive({ input$manifest_tbl_rows_selected })
    ))

  })
}
