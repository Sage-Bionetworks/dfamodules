#############################
## Schematic API Functions ##
#############################

# Functions follow best practice convention laid out in httr vignette
# https://httr.r-lib.org/articles/api-packages.html

#' Download a manifest using the dataset Synapse ID
#'
#' @param asset_view ID of view listing all project data assets. For example,
#' for Synapse this would be the Synapse ID of the fileview listing all data
#' assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param access_token Synapse login cookie, PAT, or API key.
#' @param base_url URL to schematic API endpoint
#' @export

dataset_manifest_download <- function(asset_view,
                                      dataset_id,
                                      access_token,
                                      base_url = paste0(
                                        "https://",
                                        "schematic.api.sagebionetworks.org"
                                      )) {
  # create api url
  url <- paste0(base_url, "/v1/dataset/manifest/download")

  # set up parameters for httr::get call
  params <- list(
    `access_token` = access_token,
    `asset_view` = asset_view,
    `dataset_id` = dataset_id,
    `as_json` = TRUE,
    `new_manifest_name` = NULL
  )

  # run GET
  res <- httr::GET(url = url, query = params)

  # pull out content from request
  parsed <- suppressMessages(
    jsonlite::fromJSON(httr::content(res, as = "text"))
  )

  # if the api call returns an error
  # surface error to user
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "Schematic API request failed [%s]\n%s",
        httr::status_code(res),
        parsed$detail
      ),
      call. = FALSE
    )
  }

  # return a helpful object
  structure(
    list(
      content = parsed,
      response = res
    ),
    class = "schematic_api"
  )
}

#' schematic rest api to submit metadata
#'
#' @param data_type Type of dataset. Set to None for no validation check
#' @param asset_view ID of view listing all project data assets. For example,
#' for Synapse this would be the Synapse ID of the fileview listing all data
#' assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param file_name Filepath of csv to validate
#' @param access_token Synapse login cookie, PAT, or API key
#' @param restrict_rules If True, validation suite will only run with in-house
#' validation rule. If False, the Great Expectations suite will be utilized and
#' all rules will be available.
#' @param manifest_record_type Manifest storage type. Options: "--", "table"
#' (default), "entity", "both".
#' @param base_url URL to schematic API endpoint
#' @param schema_url URL to a schema jsonld
#'
#' @returns TRUE if successful upload or validate errors if not.
#' @export

model_submit <- function(data_type = NULL,
                         asset_view,
                         dataset_id,
                         file_name,
                         access_token,
                         restrict_rules = TRUE,
                         manifest_record_type = "file_only",
                         base_url = paste0(
                           "https://",
                           "schematic.api.sagebionetworks.org"
                         ),
                         schema_url,
                         use_schema_label = TRUE) {
  # create url
  url <- paste0(base_url, "/v1/model/submit")

  # set up parameters for httr::get call
  params <- list(
    `schema_url` = schema_url,
    `data_type` = data_type,
    `dataset_id` = dataset_id,
    `manifest_record_type` = manifest_record_type,
    `restrict_rules` = restrict_rules,
    `asset_view` = asset_view,
    `access_token` = access_token,
    `use_schema_label` = use_schema_label
  )

  files <- list(
    `file_name` = httr::upload_file(file_name)
  )

  # POST
  res <- httr::POST(
    url = url,
    query = params,
    body = files
  )

  # parse response for content
  parsed <- httr::content(res)

  # if the api call returns an error
  # surface error to user
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "Schematic API request failed [%s]",
        httr::status_code(res)
      ),
      call. = FALSE
    )
  }

  # return a helpful object
  structure(
    list(
      content = parsed,
      response = res
    ),
    class = "schematic_api"
  )
}

#' Gets all datasets in a given storage project
#'
#' @param asset_view synapse ID of master file view.
#' @param project_id synapse ID of a storage project.
#' @param access_token synapse PAT
#' @param base_url URL to schematic API endpoint
#'
#' @export

storage_project_datasets <- function(asset_view,
                                     project_id,
                                     access_token,
                                     base_url = paste0(
                                       "https://",
                                       "schematic.api.",
                                       "sagebionetworks.org"
                                     )) {
  # create url
  url <- paste0(base_url, "/v1/storage/project/datasets")

  # set up parameters for httr::get call
  params <- list(
    asset_view = asset_view,
    project_id = project_id,
    access_token = access_token
  )

  # GET
  res <- httr::GET(url, query = params)

  # pull out content from request
  parsed <- suppressMessages(
    jsonlite::fromJSON(httr::content(res, as = "text"))
  )

  # if the api call returns an error
  # surface error to user
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "Schematic API request failed [%s]\n%s",
        httr::status_code(res),
        parsed$detail
      ),
      call. = FALSE
    )
  }


  # return parsed matrix as dataframe
  parsed_df <- data.frame(parsed)

  if (nrow(parsed_df > 0)) {
    names(parsed_df) <- c("id", "name")
  }

  # return a helpful object
  structure(
    list(
      content = parsed_df,
      response = res
    ),
    class = "schematic_api"
  )
}

#' Get all storage projects the current user has access to
#'
#' @param asset_view synapse ID of master file view.
#' @param access_token synapse PAT
#' @param base_url URL to schematic API endpoint
#'
#' @export

storage_projects <- function(asset_view,
                             access_token,
                             base_url =
                               "https://schematic.api.sagebionetworks.org") {
  # create url
  url <- paste0(base_url, "/v1/storage/projects")

  # set up parameters for httr::get call
  params <- list(
    asset_view = asset_view,
    access_token = access_token
  )

  # GET
  res <- httr::GET(url, query = params)

  # pull out content from request
  parsed <- suppressMessages(
    jsonlite::fromJSON(httr::content(res, as = "text"))
  )

  # if the api call returns an error
  # surface error to user
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "Schematic API request failed [%s]\n%s",
        httr::status_code(res),
        parsed$detail
      ),
      call. = FALSE
    )
  }

  # return parsed matrix as dataframe
  parsed_df <- data.frame(parsed)
  names(parsed_df) <- c("id", "name")

  # return a helpful object
  structure(
    list(
      content = parsed_df,
      response = res
    ),
    class = "schematic_api"
  )
}

#' /storage/project/manifests
#'
#' @param asset_view synapse ID of master file view.
#' @param dataset_id synapse ID of a storage dataset.
#' @param access_token synapse PAT
#' @param base_url URL to schematic API endpoint
#'
#' @export

storage_project_manifests <- function(asset_view,
                                      project_id,
                                      access_token,
                                      base_url =
                                        paste0(
                                          "https://",
                                          "schematic.api.",
                                          "sagebionetworks.org"
                                        )) {
  # write URL
  url <- paste0(base_url, "/v1/storage/project/manifests")

  # set up parameters for httr::get call
  params <- list(
    `access_token` = access_token,
    `project_id` = project_id,
    `asset_view` = asset_view
  )

  # GET
  res <- httr::GET(url = url, query = params)

  # pull out content from request
  parsed <- suppressMessages(
    jsonlite::fromJSON(httr::content(res, as = "text"))
  )

  # if the api call returns an error
  # surface error to user
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "Schematic API request failed [%s]\n%s",
        httr::status_code(res),
        parsed$detail
      ),
      call. = FALSE
    )
  }

  # return parsed matrix as dataframe
  parsed_df <- data.frame(parsed)

  # if dataframe has content, name columns
  if (nrow(parsed_df) > 0) {
    names(parsed_df) <- c(
      "dataset_id",
      "manifest_id",
      "data_type",
      "folder_name",
      "file_name"
    )

    # drop redundant data type column
    parsed_df <- parsed_df[, -6]
  }

  # return a helpful object
  structure(
    list(
      content = parsed_df,
      response = res
    ),
    class = "schematic_api"
  )
}


#' Get all the attributes associated with a specific data model component
#' formatted as a dataframe
#'
#' @param schema_url A data model URL
#' @param component Component of the data model to explore
#' @param base_url URL to schematic API endpoint
#' @export

visualize_component <- function(schema_url,
                                component = "DataFlow",
                                base_url =
                                  "https://schematic.api.sagebionetworks.org") {
  # create api url
  url <- paste0(base_url, "/v1/visualize/component")

  # set up parameters for httr::get
  params <- list(
    `schema_url` = schema_url,
    `component` = component,
    `include_index` = "false"
  )

  # GET
  res <- httr::GET(url = url, query = params)

  # check that application returns json
  # even when json = TRUE, http_type = "text/csv"
  # if (httr::http_type(res) != "application/json") {
  #   stop("API did not return json", call. = FALSE)
  # }

  # pull out content from request
  parsed <- suppressMessages(httr::content(res))

  # if the api call returns an error
  # surface error to user
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "Schematic API request failed [%s]",
        httr::status_code(res)
      ),
      call. = FALSE
    )
  }

  # return a helpful object
  structure(
    list(
      content = parsed,
      response = res
    ),
    class = "schematic_api"
  )
}

#' Get all the attributes associated with a specific data model component
#' formatted as a dataframe
#'
#' @param schema_url A data model URL
#' @param node_display_name Display lable of node
#' @param base_url URL to schematic API endpoint
#' @export

schemas_get_node_validation_rules <- function(schema_url,
                                              node_display_name,
                                              base_url) {
  # api url
  url <- paste0(base_url, "/v1/schemas/get_node_validation_rules")

  # query parameters
  params <- list(
    `schema_url` = schema_url,
    `node_display_name` = node_display_name
  )

  # GET
  res <- httr::GET(url = url, query = params)

  # parse content
  parsed <- unlist(suppressMessages(httr::content(res)))

  # NULL is returned if there is no restrict rule
  # Return NA instead
  if (is.null(parsed)) {
    parsed <- NA
  }

  # if the api call returns an error
  # surface error to user
  if (httr::http_error(res)) {
    stop(
      sprintf(
        "Schematic API request failed [%s]",
        httr::status_code(res)
      ),
      call. = FALSE
    )
  }

  # return a helpful object
  structure(
    list(
      content = parsed,
      response = res
    ),
    class = "schematic_api"
  )
}

# print method for schematic_api class of functions
print.schematic_api <- function(x, ...) {
  utils::str(x$content)
  invisible(x)
}
