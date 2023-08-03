#' update_data_flow_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @export

mod_update_data_flow_status_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "Update Data Flow Status",
      width = NULL,

      # release scheduled input
      mod_scheduler_ui(ns("release_date"),
                       dateInput_label = shiny::h4("Schedule Release"),
                       checkboxInput_label = "Unschedule Release"),
      # embargo input
      mod_scheduler_ui(ns("embargo"),
                       dateInput_label = shiny::h4("Schedule Embargo"),
                       checkboxInput_label = "Unschedule Embargo"),

      # standard compliance input
      shiny::radioButtons(ns("standard_compliance"),
                          label = shiny::h4("Standard Compliance"),
                          list("TRUE" = TRUE, "FALSE" = FALSE),
                          selected = NA),

      # data portal input
      shiny::radioButtons(ns("data_portal"),
                          label = shiny::h4("Data Portal"),
                          choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                          selected = NA),

      # released input
      shiny::radioButtons(ns("released"),
                          label = shiny::h4("Released"),
                          choices = list("TRUE" = TRUE, "FALSE" = FALSE),
                          selected = NA),

      shiny::selectizeInput(ns("status"),
                         label = shiny::h4("Status"),
                         choices = list("Not Scheduled" = "not_scheduled",
                                        "Quarantine" = "quarantine",
                                        "Released" = "released"),
                         multiple = TRUE,
                         options = list(maxItems = 1)),

      shiny::br(),

      # reset button
      shiny::actionButton(ns("reset_btn"), "Reset Button")
    )
  )
}


#' update_data_flow_status Server Functions
#' @export

mod_update_data_flow_status_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    release_scheduled <- mod_scheduler_server("release_date",
                                              shiny::reactive({input$reset_btn}))

    embargo <- mod_scheduler_server("embargo",
                                    shiny::reactive({input$reset_btn}))

    shiny::observeEvent(input$reset_btn, {
      shiny::updateRadioButtons(session = session,
                                inputId = "standard_compliance",
                                selected = character(0))

      shiny::updateRadioButtons(session = session,
                                inputId = "data_portal",
                                selected = character(0))

      shiny::updateRadioButtons(session = session,
                                inputId = "released",
                                selected = character(0))
    })

    res <- shiny::reactive({
      list(release_scheduled = release_scheduled(),
           embargo = embargo(),
           standard_compliance = input$standard_compliance,
           data_portal = input$data_portal,
           released = input$released,
           status = input$status)
    })


    return(shiny::reactive({ res() }))


  })
}
