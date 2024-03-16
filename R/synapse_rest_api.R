###########################
## Synapse API Functions ##
###########################

#' @title Check Access Permissions to a Synapse Entity
#' @description wrapper for https://rest-docs.synapse.org/rest/GET/entity/id/access.html
#'
#' @param url URL to REST API endpoint
#' @param id Synapse ID
#' @param access Access Type to check
#' @param auth Synapse authentication token
#'
#' @export
synapse_access <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/entity",
                           id,
                           access,
                           auth) {
  if (is.null(id)) stop("id cannot be NULL")
  req_url <- file.path(url, id, "access")
  req <- httr::GET(req_url,
    httr::add_headers(Authorization = paste0("Bearer ", auth)),
    query = list(accessType = access)
  )

  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)

  cont <- httr::content(req)
  cont$result
}

#' @title Gets annotations for a given entity ID
#' @description wrapper for https://rest-docs.synapse.org/rest/GET/entity/id/annotations2.html
#'
#' @param url URL to REST API endpoint
#' @param id Synapse ID
#' @param auth Synapse authentication token
#'
#' @export

synapse_annotations <- function(id,
                                auth,
                                url = "https://repo-prod.prod.sagebase.org/repo/v1/entity") {
  if (is.null(id)) stop("id cannot be NULL")
  req_url <- file.path(url, id, "annotations2")
  req <- httr::GET(
    req_url,
    httr::add_headers(Authorization = paste0("Bearer ", auth))
  )

  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)

  cont <- httr::content(req)
  return(cont$annotations)
}

#' @title Gets an annotation for a given entity ID
#'
#' @param id Synapse ID
#' @param annotation label Label of annotation (case sensitive)
#' @param access_token Synapse authentication token
#' @param na_replace NA replacement string
#' @param url Synapse API endpoint URL
#'
#' @export

get_annotations <- function(id,
                            annotation_label,
                            na_replace = NA,
                            access_token,
                            url = file.path(
                              "https://repo-prod.prod.sagebase.org",
                              "repo/v1/entity")
) {

  # query annotations synapse endpoint
  annotations_out <- synapse_annotations(
    id = project_id,
    auth = access_token,
    url = url
  )

  annotations_out[annotation_label]

  # check that studyStatus is a returned annotation
  # if it is return it's value, if it isn't return na_replace
  if (!is.null(annotations_out[[annotation_label]])) {
    annotation <- toupper(unlist(annotations_out[[annotation_label]]$value))
  } else {
    annotation <- na_replace
  }

  return(annotation)
}
