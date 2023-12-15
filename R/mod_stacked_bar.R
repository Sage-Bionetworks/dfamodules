#' Stacked Bar Plot UI Function
#'
#' @description A Shiny module that takes in a datafame and outputs a stacked bar plot.
#' @param id shiny id
#' @importFrom shiny NS tagList
#' @export

mod_stacked_bar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("stacked_bar"))
  )
}

#' Stacked Bar Plot Server Functions
#'
#' @param id shiny id
#' @param df A data frame containing data to plot
#' @param x_var Column name of X variable
#' @param y_var Column name of Y variable
#' @param fill_var Column name of fill variable
#' @param title Title of plot
#' @param x_lab X axis label
#' @param y_lab Y axis label
#' @param colors Fill colors
#' @param x_line Add a line across x axis
#' @param width Width of stacked bars
#' @param date_breaks X axis date breaks
#' @param coord_flip Flip coordinates 90 degrees
#' @export

mod_stacked_bar_server <- function(id,
                                   df,
                                   x_var,
                                   y_var,
                                   fill_var,
                                   title = NULL,
                                   x_lab = NULL,
                                   y_lab = NULL,
                                   colors = NULL,
                                   x_line = NULL,
                                   width = NULL,
                                   date_breaks = NULL,
                                   coord_flip = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # render stacked bar
    output$stacked_bar <- shiny::renderPlot({
      df <- df()

      # base plot
      bar <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
        ggplot2::geom_col(position = "fill", width = width)

      # add x intercept
      if (!is.null(x_line)) {
        bar <- bar +
          ggplot2::geom_vline(xintercept = x_line, linetype = 2, colour = "black")
      }

      # scale x axis with date breaks
      if (!is.null(date_breaks)) {
        bar <- bar +
          ggplot2::scale_x_date(date_breaks = date_breaks)
      }

      # add styling/labs
      bar <- bar +
        ggplot2::labs(title = title, x = x_lab, y = y_lab) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

      # add custom color
      if (!is.null(colors)) {
        bar <- bar +
          ggplot2::scale_fill_manual(values = colors)
      }

      # flip coordinates
      if (coord_flip) {
        bar <- bar +
          ggplot2::coord_flip()
      }

      bar
    })
  })
}

## To be copied in the UI
# mod_stacked_bar_ui("stacked_bar_1")

## To be copied in the server
# mod_stacked_bar_server("stacked_bar_1")
