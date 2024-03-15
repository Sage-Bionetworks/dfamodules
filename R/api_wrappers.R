###################################################################
## Functions that build on those defined in schematic_rest_api.R ##
###################################################################

#' Call `storage/project/manifests` Schematic endpoint for every project in a given asset_view
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param na_replace NA replacement string
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API
#' @param verbose T/F for console messages
#'
#' @export

get_all_manifests <- function(asset_view,
                              na_replace = NA,
                              access_token,
                              base_url = "https://schematic.api.sagebionetworks.org",
                              verbose = FALSE) {
  if (verbose) {
    message(paste0("Getting storage project list for ", asset_view))
  }

  # get all storage projects under asset view
  sp_obj <- storage_projects(
    asset_view = asset_view,
    access_token = access_token,
    base_url = base_url
  )

  if (verbose) {
    message(paste0("Getting manifests for ", nrow(sp_obj$content), " storage project(s)"))
  }

  synapse_manifests_list <- lapply(1:nrow(sp_obj$content), function(i) {

    sp_id <- sp_obj$content[i, "id"]
    sp_name <- sp_obj$content[i, "name"]

    if (verbose) {
      message(paste0("Retrieving manifests for ", sp_name))
    }

    manifests <- tryCatch(
      {
        storage_project_manifests(
          asset_view = asset_view,
          project_id = sp_id,
          access_token = access_token,
          base_url = base_url
        )
      },
      error = function(e) {
        message("Could not return storage/project/manifest for ", sp_id)
        message(e)
        return(NULL)
      }
    )

    if ( any(is.null(manifests$content), nrow(manifests$content) < 0) ) {

      return(NULL)

    } else {

      # pull together in a dataframe
      return(
        data.frame(
          Component = rep("DataFlow", nrow(manifests$content)),
          contributor = rep(sp_name, nrow(manifests$content)),
          contributor_id = rep(sp_id, nrow(manifests$content)),
          dataset_id = manifests$content$dataset_id,
          dataset_name = manifests$content$folder_name,
          dataset_type = manifests$content$data_type
          )
        )
    }

  })

  all_manifests <- do.call("rbind", synapse_manifests_list)

  if ( !is.null(na_replace) ) {
    all_manifests[ is.na(all_manifests) | all_manifests == "" ] <- na_replace
  }

  # return dataframe
  return(all_manifests)
}


#' Call `calculate_items_per_manifest` calculate the number of items per manifest synID in a given dataframe
#'
#' @param df A dataframe with `dataset_id` and `dataset_type` columns from data flow status manifest
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param na_replace NA replacement string
#' @param access_token Synapse PAT
#' @param verbose default FALSE
#' @param base_url Base URL of schematic API
#'
#' @export

calculate_items_per_manifest <- function(df,
                                         asset_view,
                                         na_replace = NA,
                                         access_token,
                                         verbose = FALSE,
                                         base_url) {
  # create progress bar
  if (verbose) {
    message("Counting num items per manifest. This may take a minute.")
    pb <- utils::txtProgressBar(min = 0, max = nrow(df), initial = 0, style = 3)
  }

  sapply(1:nrow(df), function(i) {

    # dataset == "" indicates that there is no manifest
    if (df$dataset_type[i] == "" | is.na(df$dataset_type[i])) {

      manifest_nrow <- na_replace

      # update progress bar
      if (verbose) {
        utils::setTxtProgressBar(pb,i)
      }

    } else {

      # download manifest
      manifest <- tryCatch(
        {
          dataset_manifest_download(asset_view = asset_view,
                                    dataset_id = df[i, "dataset_id"],
                                    access_token = access_token,
                                    base_url = base_url)
        },
        error=function(e) {
          return(NULL)
        }
      )

      # if no manifest is downloaded, return NA
      # otherwise count rows and return nrow
      manifest_nrow <- ifelse(is.null(manifest$content), na_replace, nrow(manifest$content))

      # update progress bar
      if (verbose) {
        utils::setTxtProgressBar(pb,i)
      }
    }

    return(manifest_nrow)
  })
}
