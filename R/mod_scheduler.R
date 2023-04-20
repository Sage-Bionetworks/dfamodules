#' scheduler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param dateInput_label label for dateInput
#' @param checkboxInput_label date for checkboxInput
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export

mod_scheduler_ui <- function(id,
                             dateInput_label,
                             checkboxInput_label) {
  ns <- NS(id)
  tagList(

    # initialize shinyjs
    shinyjs::useShinyjs(),

    # release scheduled input
    dateInput(ns("date"), label = dateInput_label, value = NA),
    checkboxInput(ns("unschedule_chk"), label = checkboxInput_label, value = FALSE))
}

#' scheduler Server Functions
#'
#' @noRd
#' @export

mod_scheduler_server <- function(id,
                                 reset_btn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # on checkbox click
    observeEvent(input$unschedule_chk, {

      # if the checkbox is clicked:
      if(input$unschedule_chk == TRUE){

        # update the date input to blank out the date
        updateDateInput(session = session, inputId = "date", value = NA)

        # use shinyjs to disable the date ui widget
        shinyjs::disable("date")

      } else {

        # when checkbox is not clicked enable the date box to be used
        shinyjs::enable("date")
      }
    })

    observeEvent(reset_btn(), {
      updateDateInput(session = session, inputId = "date", value = NA)
    })

    date_out <- reactive({

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

    return(reactive({ date_out() }))

  })
}

## To be copied in the UI
# mod_scheduler_ui("scheduler_1")

## To be copied in the server
# mod_scheduler_server("scheduler_1")
