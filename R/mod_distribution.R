#' Distribution Plot UI Function
#'
#' @description A Shiny module that takes a dataframe and displays a distribution bar plot
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns("distribution_plot"))
  )
}
    
#' Distribution Plot Server Functions
#' 
#' @param id Shiny ID to call server module
#' @param df A data frame containing data to plot
#' @param group_by_var Column variable to group data by (x axis variable)
#' @param title Title of plot
#' @param x_lab X axis label
#' @param y_lab Y axis label
#' @param fill Fill color
#' 
#' @noRd 
mod_distribution_server <- function(id,
                                    df,
                                    group_by_var,
                                    title = NULL,
                                    x_lab = NULL,
                                    y_lab = NULL,
                                    fill = "#0d1c38"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    grouped_data <- reactive({
      df <- df()
      
      # group and tally data
      df %>%
        dplyr::group_by(.data[[group_by_var]]) %>%
        dplyr::tally()
    })
    
    
    
    # create distribution plot
    dist <- reactive({
      plot_df <- grouped_data()
      ggplot2::ggplot(plot_df,
                      ggplot2::aes(x = reorder(.data[[group_by_var]], -n, ), y = n)) +
        
        ggplot2::geom_bar(stat = "identity", fill = fill) +
        
        ggplot2::labs(title = title, x = x_lab, y = y_lab) +
        
        ggplot2::theme_minimal() +
        
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90,hjust=1)) 
    })
    
    # render plot
    output$distribution_plot <- shiny::renderPlot({
      dist()
    })
  })
}
    
## To be copied in the UI
# mod_distribution_ui("distribution_1")
    
## To be copied in the server
# mod_distribution_server("distribution_1")
