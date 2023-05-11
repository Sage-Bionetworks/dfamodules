###################################################################
## Functions that build on those defined in schematic_rest_api.R ##
###################################################################

#' Call `storage/project/manifests` Schematic endpoint for a given asset_view
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API
#' @param verbose T/F for console messages
#'
#' @export

get_all_manifests <- function(asset_view,
                              access_token,
                              base_url = "https://schematic-dev.api.sagebionetworks.org",
                              verbose = FALSE) {
  if (verbose) {
    message(paste0("Getting storage project list for ", asset_view))
  }

  # get all storage projects under asset view
  sp_obj <- storage_projects(asset_view = asset_view,
                             access_token = access_token,
                             base_url = base_url)

  if (verbose) {
    message(paste0("Getting manifests for ", nrow(sp_obj$content), " storage project(s)"))
  }

  synapse_manifests_list <- lapply(1:nrow(sp_obj$content), function(i) {

    sp_id <- sp_obj$content[i, "id"]
    sp_name <- sp_obj$content[i, "name"]

    if (verbose) {
      message(paste0("Retrieving manifests for ", sp_name))
    }

    manifests <- storage_project_manifests(asset_view = asset_view,
                                           project_id = sp_id,
                                           access_token = access_token,
                                           base_url = base_url)

    # if manifest has
    if (nrow(manifests$content) > 0) {

      # pull together in a dataframe
      return(data.frame(Component = rep("DataFlow", nrow(manifests$content)),
                        contributor = rep(sp_name, nrow(manifests$content)),
                        entityId = manifests$content$dataset_id,
                        dataset_name = manifests$content$folder_name,
                        dataset = manifests$content$data_type))
    } else {
      return(NULL)
    }
  })

  # return dataframe
  return(do.call("rbind", synapse_manifests_list))
}


#' Call `calculate_items_per_manifest` calculate the number of items per manifest synID in a given dataframe
#'
#' @param df A dataframe with `entityId` and `dataset` columns from data flow status manifest
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API
#'
#' @export

calculate_items_per_manifest <- function(df,
                                         asset_view,
                                         access_token,
                                         base_url) {

  sapply(1:nrow(df), function(i) {

    # dataset == "" indicates that there is no manifest
    if (df$dataset[i] == "") {

      manifest_nrow <- "Not Applicable"

      } else {

      # download manifest
      manifest <- tryCatch(
        {
          manifest_download(asset_view = asset_view,
                            dataset_id = df[i, "entityId"],
                            access_token = access_token,
                            base_url = base_url)
        },
        error=function(e) {
          return(NULL)
        }
      )

      # if no manifest is downloaded, return NA
      # otherwise count rows and return nrow
      manifest_nrow <- ifelse(is.null(manifest$content), "Not Applicable", nrow(manifest$content))
    }

    return(manifest_nrow)
  })
}
