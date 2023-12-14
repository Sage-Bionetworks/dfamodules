#' Distribution Plot UI Function
#'
#' @description A Shiny module that takes a dataframe and displays a distribution bar plot
#'
#' @param id shiny id
#' @importFrom shiny NS tagList
#' @export

mod_distribution_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("distribution_plot"))
  )
}

#' Distribution Plot Server Functions
#'
#' @param id shiny id
#' @param df A data frame containing data to plot
#' @param group_by_var Column variable to group data by (x axis variable)
#' @param title Title of plot
#' @param x_lab X axis label
#' @param y_lab Y axis label
#' @param fill Fill color
#' @export

mod_distribution_server <- function(id,
                                    df,
                                    group_by_var,
                                    title = NULL,
                                    x_lab = NULL,
                                    y_lab = NULL,
                                    fill = "#0d1c38") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    grouped_data <- shiny::reactive({
      df <- df()

      # group and tally data
      df %>%
        dplyr::group_by(.data[[group_by_var]]) %>%
        dplyr::tally()
    })



    # create distribution plot
    dist <- shiny::reactive({
      plot_df <- grouped_data()
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = stats::reorder(.data[[group_by_var]], -n, ), y = n)
      ) +
        ggplot2::geom_bar(stat = "identity", fill = fill) +
        ggplot2::labs(title = title, x = x_lab, y = y_lab) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    })

    # render plot
    output$distribution_plot <- shiny::renderPlot({
      dist()
    })
  })
}
