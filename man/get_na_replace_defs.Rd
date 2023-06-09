% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{get_na_replace_defs}
\alias{get_na_replace_defs}
\title{Parse config to get na replacement definitions with custom javascript. Outputs a list.}
\usage{
get_na_replace_defs(prepped_dataframe, config)
}
\arguments{
\item{prepped_dataframe}{dataframe output by \code{prep_df_for_dash()}}

\item{config}{datatable_dashboard_config.json as a datatable (\code{jsonlite::read_json("inst/datatable_dashboard_config.json")})}
}
\description{
Parse config to get na replacement definitions with custom javascript. Outputs a list.
}
