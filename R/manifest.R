#' Prepare a dataframe that has been downloaded from Synapse for the Data Flow App
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#'
#' @export

prep_manifest_dfa <- function(manifest,
                              config) {

  # convert "Not Applicable" to NA
  manifest[ manifest == "Not Applicable" ] <- NA

  # convert contribute and dataset to factor
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("drop_down_filter", config),
                                  type = "factor")

  # num_items to integer column
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("integer", config),
                                  type = "integer")

  # release_scheduled and embargo to date columns
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("date", config),
                                  type = "date")

  return(manifest)
}

#' Prepare a dataframe for Synapse submission
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#'
#' @export

prep_manifest_submit <- function(manifest,
                                 config) {

  # convert columns back to string
  col_names <- c(get_colname_by_type("date", config),
                 get_colname_by_type("drop_down_filter", config),
                 get_colname_by_type("integer", config))

  manifest <- convert_column_type(df = manifest,
                                  col_names = col_names,
                                  type = "character")

  # convert NA to "Not Applicable"
  manifest[ is.na(manifest) ] <- "Not Applicable"

  return(manifest)
}

#' Convert a list to a dataframe
#'
#' @param dfs_status_manifest A data flow status manifest
#' @param dfs_updates Output from mod_update_data_flow_status.R
#' @param selected_datasets_df Output from mod_dataset_selection.R
#'
#' @export

update_dfs_manifest <- function(dfs_manifest,
                                dfs_updates,
                                selected_datasets_df) {

  # remove unchanged attributes from selections
  dfs_updates <- dfs_updates[!unlist(lapply(dfs_updates, is.null))]

  # capture column names to update
  col_names <- names(dfs_updates)

  # loop over the list of changed attributes
  # for each attribute:
  #   - pull out the original vector
  #   - get the updated entry from the list of attributes
  #   - apply the entry to the selected datasets in dfs manifest
  dfs_manifest[col_names] <- lapply(col_names, function(x) {

    # pull out column into a vector
    vec <- dfs_manifest[[x]]

    # get entry from updated data flow status attributes list
    entry <- dfs_updates[[x]]

    # update vector by index
    manifest_selected_idx <- match(selected_datasets_df$id, dfs_manifest$entityId)
    vec[manifest_selected_idx] <- entry

    return(vec)

  })

  return(dfs_manifest)
}

#' Generate a data flow status manifest skeleton. Fills in the component, contributor, data type, number of items, and dataset name columns.
#'
#' @param storage_project_list List output from `storage_projects` schematic endpoint
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param calc_num_items TRUE/FALSE. Calculate the number of items in each manifest.
#' @param base_url Base URL of schematic API
#'
#' @export

generate_data_flow_manifest_skeleton <- function(asset_view,
                                                 input_token,
                                                 calc_num_items,
                                                 base_url = "https://schematic-dev.api.sagebionetworks.org") {

  # get manifests for each storage project
  dfs_manifest <- get_all_manifests(asset_view = asset_view,
                                    input_token = input_token,
                                    base_url = base_url,
                                    verbose = TRUE)

  # count rows in each manifest listed
  if (calc_num_items) {

    num_items <- calculate_items_per_manifest(df = dfs_manifest,
                                              asset_view = asset_view,
                                              input_token = input_token,
                                              base_url = base_url)

    # if calc_num_itmes = false, just fill in the column with Not Applicable
  } else {
    num_items <- rep("Not Applicable", nrow(dfs_manifest))
  }

  # add to manifest
  dfs_manifest$num_items <- num_items

  # add missing columns
  # FIXME: Remove hardcoded column names
  # This function will break if dataflow schema changes
  # Source column names from schema?
  dfs_manifest$release_scheduled <- rep("Not Applicable", nrow(dfs_manifest))
  dfs_manifest$embargo <- rep("Not Applicable", nrow(dfs_manifest))
  dfs_manifest$standard_compliance <- rep(FALSE, nrow(dfs_manifest))
  dfs_manifest$data_portal <- rep(FALSE, nrow(dfs_manifest))
  dfs_manifest$released <- rep(FALSE, nrow(dfs_manifest))

  # update empty cells to "Not Applicable"
  dfs_manifest[ dfs_manifest == "" ] <- "Not Applicable"

  return(dfs_manifest)
}

#' Check synapse for updates to data flow status manifest
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param manifest_dataset_id Dataset ID for data flow status manifest to be updated
#' @param input_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)
#'
#' @export

update_data_flow_manifest <- function(asset_view,
                                      manifest_dataset_id,
                                      input_token,
                                      base_url) {

  print(paste0("Checking asset view ", asset_view, " for updates"))
  print(paste0("Getting data flow status manifest"))

  # get current data flow manifest
  dataflow_manifest_obj <- tryCatch(
    {
      manifest_download(asset_view = asset_view,
                        dataset_id = manifest_dataset_id,
                        base_url = base_url,
                        input_token = input_token)
    },
    error = function(e) {
      message("manifest_download failed")
      message(e)
    }
  )

  dataflow_manifest <- dataflow_manifest_obj$content

  # if uuid remove
  if(any(grepl("Uuid", names(dataflow_manifest)))) {
    idx <- grep("Uuid", names(dataflow_manifest))
    dataflow_manifest <- dataflow_manifest[,-idx]
  }

  # get all manifests for each storage project
  print(paste0("Getting all manifests under asset view ", asset_view, " from Synapse"))
  synapse_manifests <- tryCatch(
    {
      get_all_manifests(asset_view = asset_view,
                        input_token = input_token,
                        base_url = base_url,
                        verbose = FALSE)
    },
    error = function(e) {
      message("get_all_manifests failed")
      message(e)
    }
  )

  print("Checking data flow manifest for updates")

  # check synapse for new datasets
  dataflow_manifest_updated <- update_manifest_add_datasets(dataflow_manifest = dataflow_manifest,
                                                            get_all_manifests_out = synapse_manifests,
                                                            asset_view = asset_view,
                                                            input_token = input_token,
                                                            base_url = base_url)

  # check synapse for removed datasets
  dataflow_manifest_updated <- update_manifest_remove_datasets(dataflow_manifest = dataflow_manifest_updated,
                                                               get_all_manifests_out = synapse_manifests,
                                                               asset_view = asset_view,
                                                               input_token = input_token,
                                                               base_url = base_url)

  # check synapse for updates to dataset_name column
  dataflow_manifest_updated <- update_manifest_column(dataflow_manifest = dataflow_manifest_updated,
                                                      get_all_manifests_out = synapse_manifests,
                                                      update_column = "dataset_name",
                                                      asset_view = asset_view,
                                                      recalc_num_items = FALSE,
                                                      input_token = input_token,
                                                      base_url = base_url)

  # check synapse for updates to dataset column
  dataflow_manifest_updated <- update_manifest_column(dataflow_manifest = dataflow_manifest_updated,
                                                      get_all_manifests_out = synapse_manifests,
                                                      update_column = "dataset",
                                                      asset_view = asset_view,
                                                      recalc_num_items = TRUE,
                                                      input_token = input_token,
                                                      base_url = base_url)

  # compare updated dataflow manifest to initial manifest

  changes_made <- !identical(dataflow_manifest, dataflow_manifest_updated)

  # if changes have been made submit to synapse
  if (changes_made) {
      # submit to synapse
      # data_type = NULL until LP can fix model/submit endpoint for large manifests
      # If no datatype indicated no validation will be done
      message("submitting manifest to Synapse")

      # create manifest directory if it doesn't exist yet
      if (!file.exists("./manifest/")) {
        dir.create("./manifest/")
      }

      # write to csv for submission
      file_path <- "./manifest/synapse_storage_manifest_dataflow.csv"
      utils::write.csv(dataflow_manifest_updated, file_path, row.names = FALSE)

      # submit to synapse
      model_submit(data_type = NULL,
                   asset_view = asset_view,
                   dataset_id = manifest_dataset_id,
                   file_name = file_path,
                   restrict_rules = TRUE,
                   input_token = input_token,
                   manifest_record_type = "table_and_file",
                   base_url = base_url,
                   schema_url = "https://raw.githubusercontent.com/Sage-Bionetworks/data_flow/main/inst/data_flow_component.jsonld")
  } else {
    print("No updates to manifest required at this time")
  }

}

#' Update manifest with new datasets found in Synapse
#'
#' @param dataflow_manifest A dataFlow manifest
#' @param get_all_manifests_out The output of get_all_manifests. Also can be a dataframe that includes Component, contributor, entityId, dataset_name, and dataset.
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param input_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)

update_manifest_add_datasets <- function(dataflow_manifest,
                                         get_all_manifests_out,
                                         asset_view,
                                         input_token,
                                         base_url) {

  # check for new datasets by entityId
  new_datasets <- get_all_manifests_out[!get_all_manifests_out$entityId %in% dataflow_manifest$entityId,]

  # if there are new datasets...
  if (nrow(new_datasets) > 0) {

    print(paste0(nrow(new_datasets), " new dataset(s) found on Synapse"))

    # calculate number of items in each manifest
    num_items <- tryCatch(
      {
        calculate_items_per_manifest(df = new_datasets,
                                     asset_view = asset_view,
                                     input_token = input_token,
                                     base_url = base_url)
      },
      error = function(e) {
        message("num_items calculation failed")
        message(e)
      }
    )

    # fill data flow manifest rows for missing datasets
    # FIXME: Remove hardcoded column names
    # This function will break if dataflow schema changes
    # Source column names from schema?
    new_datasets$release_scheduled <- rep("Not Applicable", nrow(new_datasets))
    new_datasets$embargo <- rep("Not Applicable", nrow(new_datasets))
    new_datasets$standard_compliance <- rep(FALSE, nrow(new_datasets))
    new_datasets$data_portal <- rep(FALSE, nrow(new_datasets))
    new_datasets$released <- rep(FALSE, nrow(new_datasets))
    new_datasets$num_items <- num_items

    # remove uuid col (prep for rbind)
    if (any(grepl("Uuid", names(dataflow_manifest)))) {
      uuid_idx <- grep("Uuid", names(dataflow_manifest))
      dataflow_manifest <- dataflow_manifest[, -uuid_idx]
    }

    # bind together new dataset rows and data flow manifest
    dataflow_manifest <- rbind(dataflow_manifest, new_datasets)

    # rearrange data flow manifest
    dataflow_manifest <- dataflow_manifest %>%
      dplyr::group_by(contributor) %>%
      dplyr::arrange(contributor)
  }

  return(data.frame(dataflow_manifest))

}

#' Remove datasets that are no longer found in Synapse
#'
#' @param dataflow_manifest A dataFlow manifest
#' @param get_all_manifests_out The output of get_all_manifests. Also can be a dataframe that includes Component, contributor, entityId, dataset_name, and dataset.
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param input_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)

update_manifest_remove_datasets <- function(dataflow_manifest,
                                            get_all_manifests_out,
                                            asset_view,
                                            input_token,
                                            base_url) {

  # check for removed datasets
  remove_idx <- dataflow_manifest$entityId %in% get_all_manifests_out$entityId

  # if any of the rows are flagged for removal print a message and remove from manifest
  if (any(!remove_idx)) {
    n_remove <- sum(!remove_idx)
    print(paste0(n_remove, " dataset(s) removed from Synapse"))

    dataflow_manifest <- dataflow_manifest[remove_idx,]
  }

  return(dataflow_manifest)
}

#' Update dataFlow manifest when dataset folder name changes
#'
#' @param dataflow_manifest A dataFlow manifest
#' @param get_all_manifests_out The output of get_all_manifests. Also can be a dataframe that includes Component, contributor, entityId, dataset_name, and dataset.
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param update_column Column name of the column to be updated
#' @param recalc_num_items TRUE/FALSE if there is an item to be updated, should the manifest
#' @param input_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)

update_manifest_column <- function(dataflow_manifest,
                                   get_all_manifests_out,
                                   update_column,
                                   asset_view,
                                   recalc_num_items = FALSE,
                                   input_token,
                                   base_url) {

  # arrange by entityId
  dataflow_manifest <- dplyr::arrange(dataflow_manifest, entityId)
  get_all_manifests_out <- dplyr::arrange(get_all_manifests_out, entityId)

  # get logical index of which items have changed
  idx <- dataflow_manifest[,update_column] != get_all_manifests_out[, update_column]

  # if any items have changed update dataset type column
  if (any(idx)) {
    n_changed <- sum(idx)
    print(paste0("Making ", n_changed, " update(s) to ", update_column, " column"))
    dataflow_manifest[idx, update_column] <- get_all_manifests_out[idx, update_column]

    # if recalc_num_items = TRUE recalculate number of items in the manifest for updated items
    if (recalc_num_items) {
      dataflow_manifest$num_items[idx] <- calculate_items_per_manifest(df = dataflow_manifest[idx,],
                                                                       asset_view = asset_view,
                                                                       input_token = input_token,
                                                                       base_url = base_url)
    }
  }

  # rearrange data flow manifest
  dataflow_manifest <- dataflow_manifest %>%
    dplyr::group_by(contributor) %>%
    dplyr::arrange(contributor)

  return(data.frame(dataflow_manifest))
}
