#' update_data_flow_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#' @importFrom shiny NS tagList
#' @export

mod_update_data_flow_status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "Update Data Flow Status",
      width = NULL,

      # scheduled_release_date widget
      mod_scheduler_ui(
        ns("release_date"),
        dateInput_label = shiny::h4("Schedule Release"),
        checkboxInput_label = "Unschedule Release"
      ),

      shiny::selectInput(
        ns("status"),
        label = shiny::h4("Status"),
        choices = c("",
                    "not uploaded",
                    "uploaded",
                    "curated",
                    "quarantine",
                    "preprocessing",
                    "scheduled for release",
                    "ready for released",
                    "released")
      ),

      # released_destinations widget
      shiny::selectInput(
        ns("released_destinations"),
        label = shiny::h4("Released Destination"),
        choices = c("",
                    "data portal",
                    "dbGaP")
      ),

      # metadata_check widget
      shiny::radioButtons(
        ns("metadata_check"),
        label = shiny::h4("Metadata Check?"),
        choices = list("TRUE" = TRUE, "FALSE" = FALSE),
        selected = NA
      ),

      # governance_compliance widget
      shiny::radioButtons(
        ns("governance_compliance"),
        label = shiny::h4("Governance Compliance?"),
        list("TRUE" = TRUE, "FALSE" = FALSE),
        selected = NA
      ),

      # released widget
      shiny::radioButtons(
        ns("released"),
        label = shiny::h4("Released?"),
        choices = list("TRUE" = TRUE, "FALSE" = FALSE),
        selected = NA
      ),

      shiny::br(),

      # reset button
      shiny::actionButton(ns("reset_btn"), "Reset Button")
    )
  )
}


#' update_data_flow_status Server Functions
#' @param id shiny id
#' @export

mod_update_data_flow_status_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## SCHEDULER MODULE SERVER ---------
    release_scheduled <- mod_scheduler_server(
      "release_date",
      shiny::reactive({
        input$reset_btn
      })
    )

    ## RESET WIDGETS ---------
    shiny::observeEvent(input$reset_btn, {

      shiny::updateRadioButtons(
        session = session,
        inputId = "governance_compliance",
        selected = character(0)
      )

      shiny::updateRadioButtons(
        session = session,
        inputId = "metadata_check",
        selected = character(0)
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "status",
        selected = character(0)
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "released_destinations",
        selected = character(0)
      )

      shiny::updateRadioButtons(
        session = session,
        inputId = "released",
        selected = character(0)
      )
    })

    # BLANK STRING HANDLING --------
    status_modified <- reactive({
      print(nchar(input$status))
      if ( nchar(input$status) >= 1 ) {
        return(input$status)
      } else {
        return(NULL)
      }
    })

    release_destinations_modified <- reactive({
      if ( nchar(input$released_destinations) >= 1 ) {
        return(input$released_destinations)
      } else {
        return(NULL)
      }
    })

    return(
      shiny::reactive({
        list(
          release_scheduled = release_scheduled(),
          governance_compliance = input$governance_compliance,
          metadata_check = input$metadata_check,
          status = status_modified(),
          released_destinations = release_destinations_modified(),
          released = input$released
        )
      }))
  })
}
