#' scheduler UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#' @param dateInput_label label for dateInput
#' @param checkboxInput_label date for checkboxInput
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs disable enable
#' @export

mod_scheduler_ui <- function(id,
                             dateInput_label,
                             checkboxInput_label) {
  ns <- shiny::NS(id)
  shiny::tagList(

    # initialize shinyjs
    shinyjs::useShinyjs(),

    # release scheduled input
    shiny::dateInput(ns("date"), label = dateInput_label, value = NA),
    shiny::checkboxInput(ns("unschedule_chk"), label = checkboxInput_label, value = FALSE)
  )
}

#' scheduler Server Functions
#' @param id shiny id
#' @param reset_btn reset button input
#'
#' @export

mod_scheduler_server <- function(id,
                                 reset_btn) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # on checkbox click
    shiny::observeEvent(input$unschedule_chk, {
      # if the checkbox is clicked:
      if (input$unschedule_chk == TRUE) {
        # update the date input to blank out the date
        shiny::updateDateInput(session = session, inputId = "date", value = NA)

        # use shinyjs to disable the date ui widget
        shinyjs::disable("date")
      } else {
        # when checkbox is not clicked enable the date box to be used
        shinyjs::enable("date")
      }
    })

    shiny::observeEvent(reset_btn(), {
      shiny::updateDateInput(session = session, inputId = "date", value = NA)
    })

    date_out <- shiny::reactive({
      # if there is no date selected AND checkbox is not clicked return NULL
      if (length(input$date) == 0 & input$unschedule_chk == FALSE) {
        return(NULL)

        # if there is no date selected AND checkbox is clicked return NA (unschedule)
      } else if (length(input$date) == 0 & input$unschedule_chk == TRUE) {
        return(NA)

        # if there is a selected AND checkbox is clicked return NA (unschedule)
      } else if (length(input$date) != 0 & input$unschedule_chk == TRUE) {
        return(NA)

        # if there is a selected date and checkbox is not clicked return date
      } else {
        return(input$date)
      }
    })

    return(shiny::reactive({
      date_out()
    }))
  })
}
