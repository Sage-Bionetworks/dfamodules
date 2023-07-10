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
  # manifest <- convert_column_type(df = manifest,
  #                                 col_names = get_colname_by_type("drop_down_filter", config),
  #                                 type = "factor")

  # num_items to integer column
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("int", config),
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
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param schema_url URL of a dataFlow schema
#' @param na_replace NA replacement string
#' @param calc_num_items TRUE/FALSE. Calculate the number of items in each manifest.
#' @param access_token A Synapse PAT
#' @param base_url Base URL of schematic API
#'
#' @export

generate_dataflow_manifest <- function(asset_view,
                                       schema_url,
                                       na_replace,
                                       calc_num_items,
                                       access_token,
                                       base_url = "https://schematic-dev.api.sagebionetworks.org") {

  # get manifests for each storage project
  dataflow_manifest_chunk <- get_all_manifests(asset_view = asset_view,
                                               access_token = access_token,
                                               base_url = base_url,
                                               verbose = TRUE)

  # count rows in each manifest listed
  if (calc_num_items) {

    dataflow_manifest_chunk$num_items <- calculate_items_per_manifest(df = dataflow_manifest_chunk,
                                                                      asset_view = asset_view,
                                                                      access_token = access_token,
                                                                      base_url = base_url)

    # if calc_num_itmes = false, just fill in the column with Not Applicable
  } else {
    dataflow_manifest_chunk$num_items <- rep(na_replace, nrow(dataflow_manifest_chunk))
  }

  dataflow_manifest <- fill_dataflow_manifest(dataflow_manifest_chunk = dataflow_manifest_chunk,
                                              schema_url = schema_url,
                                              na_replace = na_replace,
                                              base_url = base_url)

  return(dataflow_manifest)
}

#' Generate a data flow status manifest skeleton. Fills in the component, contributor, data type, number of items, and dataset name columns.
#'
#' @param dataflow_manifest_chunk Some rows of a data flow manifest. Generally will be `contributor`, `entityId`, `Component`, `dataset`, `dataset_name`
#' @param schema_url URL of DataFlow schema jsonld (if on GitHub must be raw file)
#' @param na_replace String to use in place of NA. Defaults to "Not Applicable". Enter NULL for `NA`.
#' @param base_url Base URL of schematic API
#'
#' @export

fill_dataflow_manifest <- function(dataflow_manifest_chunk,
                                   schema_url,
                                   na_replace = NULL,
                                   base_url) {

  # set NA if no replacement string provided
  if (is.null(na_replace)) {
    na_replace <- NA
  }

  # get attribute_df
  vc_out <- visualize_component(schema_url,
                                "DataFlow",
                                base_url)
  attributes_df <- vc_out$content

  # find attributes that are not present in provided manifest chunk
  missing_attributes_df <- attributes_df[!attributes_df$Attribute %in% names(dataflow_manifest_chunk),]

  # fill in missing attributes
  # TRUE/FALSE or Not Applicable
  # Add NA_variable

  missing_attributes_filled <- lapply(1:nrow(missing_attributes_df), function(i) {
    if (grepl("TRUE", missing_attributes_df[i, "Valid Values"])) {
      column_fill <- data.frame(rep("FALSE", nrow(dataflow_manifest_chunk)))
    } else {
      column_fill <- data.frame(rep(na_replace, nrow(dataflow_manifest_chunk)))
    }

    names(column_fill) <- missing_attributes_df$Attribute[i]

    column_fill
  }) %>%
    dplyr::bind_cols()

  # bind manifest chunks
  dataflow_manifest <- cbind(dataflow_manifest_chunk, missing_attributes_filled)

  # update empty cells to "Not Applicable"
  dataflow_manifest[ dataflow_manifest == "" ] <- na_replace

  # return filled columns with original manifest chunk
  return(dataflow_manifest)
}


#' Check synapse for updates to data flow status manifest
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param manifest_dataset_id Dataset ID for data flow status manifest to be updated
#' @param na_replace Replacement string for NA cells
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)
#'
#' @export

update_data_flow_manifest <- function(asset_view,
                                      manifest_dataset_id,
                                      na_replace = NULL,
                                      schema_url,
                                      access_token,
                                      base_url) {

  print(paste0("Checking asset view ", asset_view, " for updates"))
  print(paste0("Getting data flow status manifest"))

  # get current data flow manifest
  dataflow_manifest_obj <- tryCatch(
    {
      dataset_manifest_download(asset_view = asset_view,
                                dataset_id = manifest_dataset_id,
                                base_url = base_url,
                                access_token = access_token)
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
                        na_replace = na_replace,
                        access_token = access_token,
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
                                                            na_replace = "Not Applicable",
                                                            schema_url = schema_url,
                                                            access_token = access_token,
                                                            base_url = base_url)

  # check synapse for removed datasets
  dataflow_manifest_updated <- update_manifest_remove_datasets(dataflow_manifest = dataflow_manifest_updated,
                                                               get_all_manifests_out = synapse_manifests,
                                                               asset_view = asset_view,
                                                               access_token = access_token,
                                                               base_url = base_url)

  # check synapse for updates to dataset_name column
  dataflow_manifest_updated <- update_manifest_column(dataflow_manifest = dataflow_manifest_updated,
                                                      get_all_manifests_out = synapse_manifests,
                                                      update_column = "dataset_name",
                                                      asset_view = asset_view,
                                                      recalc_num_items = FALSE,
                                                      access_token = access_token,
                                                      base_url = base_url)

  # check synapse for updates to dataset column
  dataflow_manifest_updated <- update_manifest_column(dataflow_manifest = dataflow_manifest_updated,
                                                      get_all_manifests_out = synapse_manifests,
                                                      update_column = "dataset",
                                                      asset_view = asset_view,
                                                      recalc_num_items = TRUE,
                                                      access_token = access_token,
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
                   access_token = access_token,
                   manifest_record_type = "table_and_file",
                   base_url = base_url,
                   schema_url = schema_url)
  } else {
    print("No updates to manifest required at this time")
  }

}

#' Update manifest with new datasets found in Synapse
#'
#' @param dataflow_manifest A dataFlow manifest
#' @param get_all_manifests_out The output of get_all_manifests. Also can be a dataframe that includes Component, contributor, entityId, dataset_name, and dataset.
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)

update_manifest_add_datasets <- function(dataflow_manifest,
                                         get_all_manifests_out,
                                         asset_view,
                                         na_replace,
                                         schema_url,
                                         access_token,
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
                                     access_token = access_token,
                                     base_url = base_url)
      },
      error = function(e) {
        message("num_items calculation failed")
        message(e)
      }
    )

    new_datasets$num_items <- num_items

    # fill data flow manifest rows for missing datasets
    new_datasets <- fill_dataflow_manifest(dataflow_manifest_chunk = new_datasets,
                                           schema_url = schema_url,
                                           na_replace = na_replace,
                                           base_url = base_url)

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
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)

update_manifest_remove_datasets <- function(dataflow_manifest,
                                            get_all_manifests_out,
                                            asset_view,
                                            access_token,
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
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)

update_manifest_column <- function(dataflow_manifest,
                                   get_all_manifests_out,
                                   update_column,
                                   asset_view,
                                   recalc_num_items = FALSE,
                                   access_token,
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
                                                                       access_token = access_token,
                                                                       base_url = base_url)
    }
  }

  # rearrange data flow manifest
  dataflow_manifest <- dataflow_manifest %>%
    dplyr::group_by(contributor) %>%
    dplyr::arrange(contributor)

  return(data.frame(dataflow_manifest))
}
