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
  # QC
  if (is.null(id)) stop("id cannot be NULL")

  # create api url
  req_url <- file.path(url, id, "access")

  # get request
  req <- httr::GET(
    req_url,
    httr::add_headers(Authorization = paste0("Bearer ", auth)),
    query = list(accessType = access)
  )

  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)

  # return result
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
  # QC
  if (is.null(id)) stop("id cannot be NULL")

  # Create url
  req_url <- file.path(url, id, "annotations2")

  # make request
  req <- httr::GET(
    req_url,
    httr::add_headers(Authorization = paste0("Bearer ", auth))
  )

  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)

  # return annotations
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
    id = id,
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

#' @title Get children of a synapse entity
#' https://rest-docs.synapse.org/rest/POST/entity/children.html
#' @param url Synapse api endpoint
#' @param auth Synapse token
#' @param parentId Synapse ID of parent folder
#' @param nextPageToken Synapse next page token
#' @param includeTypes Types to return
#' @param sortBy Variable to sort by
#' @param sortDirection sort direction
#' @param includeTotalChildCount boolean include count of children
#' @param includeSumFileSizes boolean include sum of file sizes
#' @export
synapse_entity_children <- function(url = "https://repo-prod.prod.sagebase.org/repo/v1/entity/children",
                                    auth, parentId=NULL, nextPageToken=NULL, includeTypes="project", sortBy="NAME",
                                    sortDirection="ASC", includeTotalChildCount=FALSE, includeSumFileSizes=FALSE) {

  output <- list()
  req <- httr::POST(url,
                    httr::add_headers(Authorization=paste0("Bearer ", auth)),
                    body =
                      list(parentId=parentId,
                           nextPageToken=nextPageToken,
                           includeTypes=includeTypes,
                           sortBy=sortBy,
                           sortDirection=sortDirection,
                           includeTotalChildCount=includeTotalChildCount,
                           includeSumFileSizes=includeSumFileSizes),
                    encode="json")
  
  resp <- httr::content(req)
  output <- resp$page

  while (!is.null(resp$nextPageToken)) {
    req <- httr::POST(url,
                      httr::add_headers(Authorization=paste0("Bearer ", auth)),
                      body =
                        list(parentId=parentId,
                             nextPageToken=resp$nextPageToken,
                             includeTypes=includeTypes,
                             sortBy=sortBy,
                             sortDirection=sortDirection,
                             includeTotalChildCount=includeTotalChildCount,
                             includeSumFileSizes=includeSumFileSizes),
                      encode="json")
    resp <- httr::content(req)
    output <- c(output, resp$page)
  }
  dplyr::bind_rows(output)

}
