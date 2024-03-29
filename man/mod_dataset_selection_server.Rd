% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_dataset_selection.R
\name{mod_dataset_selection_server}
\alias{mod_dataset_selection_server}
\title{Select a Dataset Server Function}
\usage{
mod_dataset_selection_server(
  id,
  storage_project_id,
  asset_view,
  access_token,
  hidden_datasets = NULL,
  base_url
)
}
\arguments{
\item{id}{shiny id}

\item{storage_project_id}{Reactive value of a storage project ID}

\item{asset_view}{Fileview containing datasets}

\item{access_token}{Synapse PAT}

\item{hidden_datasets}{vector of synIds to hide}

\item{base_url}{Schematic REST API base url}
}
\description{
Select a Dataset Server Function
}
