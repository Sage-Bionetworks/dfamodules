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
                           id, access, auth) {
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
