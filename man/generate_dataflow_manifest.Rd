% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manifest.R
\name{generate_dataflow_manifest}
\alias{generate_dataflow_manifest}
\title{Generate a data flow status manifest skeleton. Fills in the component, source, data type, number of items, and dataset name columns.}
\usage{
generate_dataflow_manifest(
  asset_view,
  schema_url,
  access_token,
  na_replace = NA,
  verbose = FALSE,
  calc_num_items = TRUE,
  base_url = "https://schematic-dev.api.sagebionetworks.org"
)
}
\arguments{
\item{asset_view}{ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)}

\item{schema_url}{URL of a dataFlow schema}

\item{access_token}{A Synapse PAT}

\item{na_replace}{NA replacement string}

\item{verbose}{Show messages to help with debugging / progress bar in console}

\item{calc_num_items}{TRUE/FALSE. Calculate the number of items in each manifest.}

\item{base_url}{Base URL of schematic API}
}
\description{
Generate a data flow status manifest skeleton. Fills in the component, source, data type, number of items, and dataset name columns.
}
