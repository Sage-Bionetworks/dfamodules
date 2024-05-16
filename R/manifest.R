#' Prepare a dataframe that has been downloaded from Synapse for the Data Flow App
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#'
#' @export

prep_manifest_dfa <- function(manifest,
                              config) {

  # check that manifest and config match
  if (!all(names(config) %in% names(manifest))) {
    stop(paste0("Attributes in manifest and config do not match"))
  }

  # convert various forms of "Not Applicable" to NA
  manifest[manifest == "Not Applicable"|
             manifest == "Not applicable" |
             manifest == "not applicable" |
             manifest == ""] <- NA

  # dates come in from Synapse as char
  # convert to date type
  # use config file to source date attributes
  manifest <- convert_column_type(
    df = manifest,
    col_names = get_colname_by_type("date", config),
    type = "date"
  )

  manifest <- convert_column_type(
    df = manifest,
    col_names = get_colname_by_type("int", config),
    type = "integer"
  )

  # return the manifest
  return(manifest)
}

#' Prepare a dataframe for Synapse submission
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#' @param na_replace NA replacement string
#'
#' @export

prep_manifest_submit <- function(manifest,
                                 config,
                                 na_replace = "") {
  # convert columns back to string
  col_names <- get_colname_by_type("date", config)

  manifest <- convert_column_type(
    df = manifest,
    col_names = col_names,
    type = "character"
  )

  # convert NA to "Not Applicable"
  manifest[is.na(manifest)] <- na_replace

  return(manifest)
}

#' Apply administrator selections to Data Flow manifest
#'
#' @param dataflow_manifest A data flow status manifest
#' @param administrator_widget_output Output from mod_administrator_widgets.R
#' @param dataset_selection_module_output Output from mod_dataset_selection.R
#'
#' @export

apply_administrator_selections <- function(dataflow_manifest,
                                           administrator_widget_output,
                                           dataset_selection_module_output) {

  # remove unchanged (NULL) attributes from attributes to update
  attributes_to_update <- administrator_widget_output[!unlist(lapply(administrator_widget_output, is.null))]

  # if there are attributes to update - update them, else return manifest as is
  if (length(attributes_to_update) > 0) {
    # capture column names to update
    col_names <- names(attributes_to_update)

    # loop over the list of changed attributes
    # for each attribute:
    #   - pull out the original vector
    #   - get the updated entry from the list of attributes
    #   - apply the entry to the selected datasets in dfs manifest
    dataflow_manifest[col_names] <- lapply(col_names, function(x) {
      # pull out column into a vector
      vec <- dataflow_manifest[[x]]

      # get entry from updated data flow status attributes list
      entry <- attributes_to_update[[x]]

      # update vector by index
      manifest_selected_idx <- match(
        dataset_selection_module_output$id, dataflow_manifest$dataset_id
      )

      vec[manifest_selected_idx] <- entry

      return(vec)
    })
  }

  return(dataflow_manifest)
}

#' Generate a data flow status manifest skeleton. Fills in the component, source, data type, number of items, and dataset name columns.
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param schema_url URL of a dataFlow schema
#' @param access_token A Synapse PAT
#' @param na_replace NA replacement string
#' @param calc_num_items TRUE/FALSE. Calculate the number of items in each manifest.
#' @param base_url Base URL of schematic API
#' @param verbose Show messages to help with debugging / progress bar in console
#'
#' @export

generate_dataflow_manifest <- function(asset_view,
                                       schema_url,
                                       access_token,
                                       na_replace = NA,
                                       verbose = FALSE,
                                       calc_num_items = TRUE,
                                       base_url = "https://schematic-dev.api.sagebionetworks.org") {

  # get manifests for each storage project
  dataflow_manifest_chunk <- get_all_manifests(
    asset_view = asset_view,
    na_replace = na_replace,
    access_token = access_token,
    base_url = base_url,
    verbose = verbose
  )

  # count rows in each manifest listed
  if (calc_num_items) {
    dataflow_manifest_chunk$num_items <- calculate_items_per_manifest(
      df = dataflow_manifest_chunk,
      asset_view = asset_view,
      na_replace = na_replace,
      verbose = verbose,
      access_token = access_token,
      base_url = base_url
    )

    # if calc_num_itmes = false, just fill in the column with Not Applicable
  } else {
    dataflow_manifest_chunk$num_items <- rep(na_replace, nrow(dataflow_manifest_chunk))
  }

  dataflow_manifest <- fill_dataflow_manifest(
    dataflow_manifest_chunk = dataflow_manifest_chunk,
    schema_url = schema_url,
    na_replace = na_replace,
    access_token = access_token,
    base_url = base_url
  )

  return(dataflow_manifest)
}

#' Generate a data flow status manifest skeleton. Fills in the component, source, data type, number of items, and dataset name columns.
#'
#' @param dataflow_manifest_chunk Some rows of a data flow manifest. Generally will be `source`, `entityId`, `Component`, `dataset`, `dataset_name`
#' @param schema_url URL of DataFlow schema jsonld (if on GitHub must be raw file)
#' @param na_replace String to use in place of NA. Defaults to "Not Applicable". Enter NULL for `NA`.
#' @param base_url Base URL of schematic API
#'
#' @export

fill_dataflow_manifest <- function(dataflow_manifest_chunk,
                                   schema_url,
                                   na_replace = NA,
                                   access_token,
                                   base_url) {

  # get attribute_df
  vc_out <- visualize_component(
    schema_url = schema_url,
    component = "DataFlow",
    base_url = base_url
  )

  attributes_df <- vc_out$content

  # check for study_status attribute
  # if present add annotation to dataflow_manifest_chunk
  if ("study_status" %in% attributes_df$Attribute) {

    # study status is annotated at the project level
    # get annotations and apply to manifest
    project_ids <- unique(dataflow_manifest_chunk$source_id)

    study_status <- sapply(project_ids, function(id) {
      get_annotations(
        id = id,
        annotation_label = "studyStatus",
        na_replace = na_replace,
        access_token = access_token
        )
    })

    # Convert the named vector into a dataframe
    study_status_df <- data.frame(
      source_id = names(study_status),
      study_status = study_status
    )

    # merge study status into dataflow manifest chunk
    dataflow_manifest_chunk <- merge(
      x = dataflow_manifest_chunk,
      y = study_status_df,
      by = "source_id",
      all.x = TRUE
    )
  }
  
  # If modified_on is an attribute, get the modified_on of each manifest
  if ("modified_on" %in% attributes_df$Attribute) {
    files <- lapply(dataflow_manifest_chunk$dataset_id, function(x) {
      entities <- dfamodules::synapse_entity_children(auth = access_token,
        parentId = x, includeTypes = list("file"))
      entities$dataset_id <- x
      entities
    })
    files <- dplyr::bind_rows(files)
    if (nrow(files)) {
      files <- dplyr::filter(files, grepl("synapse_storage_manifest", name))
      files <- dplyr::select(files, modified_on=modifiedOn, dataset_id)
      dataflow_manifest_chunk <- dplyr::select(dataflow_manifest_chunk, -modified_on)
      dataflow_manifest_chunk <- merge(
        x = dataflow_manifest_chunk,
        y = files,
        by = "dataset_id",
        all.x = TRUE
      )
    }
  }

  # find attributes that are not present in provided manifest chunk
  missing_attributes_df <- attributes_df[!attributes_df$Attribute %in% names(dataflow_manifest_chunk), ]

  # fill in the rest of the attributes
  # TRUE/FALSE or NA

  missing_attributes_filled <- lapply(1:nrow(missing_attributes_df), function(i) {
    if (grepl("TRUE", missing_attributes_df[i, "Valid Values"])) {
      column_fill <- data.frame(rep(FALSE, nrow(dataflow_manifest_chunk)))
    } else {
      column_fill <- data.frame(rep(na_replace, nrow(dataflow_manifest_chunk)))
    }

    names(column_fill) <- missing_attributes_df$Attribute[i]

    column_fill
  }) %>%
    dplyr::bind_cols()

  # bind manifest chunks
  # inserts NA if there are extra cols (like entityId and Id)
  # FIXME: is this what we want to happen if data flow schema changes?
  dataflow_manifest <- dplyr::bind_cols(dataflow_manifest_chunk, missing_attributes_filled)

  # REMOVE source_id column
  source_id_idx <- grep("source_id", names(dataflow_manifest))
  dataflow_manifest <- dataflow_manifest[, -source_id_idx]

  # return filled columns with original manifest chunk
  return(dataflow_manifest)
}


#' Check synapse for updates to data flow status manifest
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param manifest_dataset_id Dataset ID for data flow status manifest to be updated
#' @param na_replace Replacement string for NA cells
#' @param schema_url A data model URL
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)
#'
#' @export

update_data_flow_manifest <- function(asset_view,
                                      manifest_dataset_id,
                                      na_replace,
                                      schema_url,
                                      access_token,
                                      base_url,
                                      verbose = TRUE) {

  verbose_message(
    m = paste0("Checking asset view ", asset_view, " for updates"),
    verbose = verbose
    )

  verbose_message(
    m = paste0("Getting data flow status manifest"),
    verbose = verbose
  )

  # get current data flow manifest
  dataflow_manifest_obj <- tryCatch(
    {
      dataset_manifest_download(
        asset_view = asset_view,
        dataset_id = manifest_dataset_id,
        base_url = base_url,
        access_token = access_token
      )
    },
    error = function(e) {
      message("manifest_download failed")
      message(e)
    }
  )

  dataflow_manifest <- dataflow_manifest_obj$content

  # make sure that num_items column is an int
  dataflow_manifest$num_items <- as.integer(dataflow_manifest$num_items)

  # get all manifests for each storage project
  verbose_message(
    m = paste0("Getting all manifests under asset view ", asset_view, " from Synapse"),
    verbose = verbose
  )

  # FOR TESTING
  synapse_manifests <- tryCatch(
    {
      get_all_manifests(
        asset_view = asset_view,
        na_replace = na_replace,
        access_token = access_token,
        base_url = base_url,
        verbose = T
      )
    },
    error = function(e) {
      message("get_all_manifests failed")
      message(e)
    }
  )
  
  files <- lapply(synapse_manifests$dataset_id, function(x) {
    entities <- dfamodules::synapse_entity_children(auth = access_token,
      parentId = x, includeTypes = list("file"))
    entities$dataset_id <- x
    entities
  })
  files <- dplyr::bind_rows(files)
  files <- dplyr::filter(files, grepl("synapse_storage_manifest_", name))
  files <- dplyr::select(files, modified_on=modifiedOn, dataset_id)
  files$modified_on <- lubridate::as_datetime(files$modified_on)
  files <- files |>
    dplyr::group_by(dataset_id) |>
    dplyr::summarise(modified_on = max(modified_on))
  files$modified_on <- as.character(files$modified_on)
  synapse_manifests <- merge(
    x = synapse_manifests,
    y = files,
    by = "dataset_id",
    all.x = TRUE
  )

  # check synapse for new datasets
  dataflow_manifest_updated <- update_manifest_add_datasets(
    dataflow_manifest = dataflow_manifest,
    get_all_manifests_out = synapse_manifests,
    asset_view = asset_view,
    na_replace = na_replace,
    schema_url = schema_url,
    access_token = access_token,
    base_url = base_url
  )

  # check synapse for removed datasets
  dataflow_manifest_updated <- update_manifest_remove_datasets(
    dataflow_manifest = dataflow_manifest_updated,
    get_all_manifests_out = synapse_manifests,
    asset_view = asset_view,
    access_token = access_token,
    base_url = base_url
  )

  # check synapse for updates to dataset_name column
  dataflow_manifest_updated <- update_manifest_column(
    dataflow_manifest = dataflow_manifest_updated,
    get_all_manifests_out = synapse_manifests,
    update_column = "dataset_name",
    asset_view = asset_view,
    recalc_num_items = FALSE,
    access_token = access_token,
    base_url = base_url
  )

  # check synapse for updates to dataset column
  dataflow_manifest_updated <- update_manifest_column(
    dataflow_manifest = dataflow_manifest_updated,
    get_all_manifests_out = synapse_manifests,
    update_column = "dataset_type",
    asset_view = asset_view,
    recalc_num_items = TRUE,
    access_token = access_token,
    base_url = base_url
  )
  
  dataflow_manifest_updated <- update_manifest_column(
    dataflow_manifest = dataflow_manifest_updated,
    get_all_manifests_out = synapse_manifests,
    update_column = "modified_on",
    asset_view = asset_view,
    recalc_num_items = FALSE,
    access_token = access_token,
    base_url = base_url
  )
  
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
    model_submit(
      data_type = NULL,
      asset_view = asset_view,
      dataset_id = manifest_dataset_id,
      file_name = file_path,
      restrict_rules = TRUE,
      access_token = access_token,
      manifest_record_type = "file_only",
      base_url = base_url,
      schema_url = schema_url
    )
  } else {
    print("No updates to manifest required at this time")
  }
}

#' Update manifest with new datasets found in Synapse
#'
#' @param dataflow_manifest A dataFlow manifest
#' @param get_all_manifests_out The output of get_all_manifests.
#'  Also can be a dataframe that includes Component, source,
#'  entityId, dataset_name, and dataset.
#' @param asset_view ID of view listing all project data assets
#' @param na_replace NA replacement text
#' @param schema_url A data model URL
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API

update_manifest_add_datasets <- function(dataflow_manifest,
                                         get_all_manifests_out,
                                         asset_view,
                                         na_replace,
                                         schema_url,
                                         access_token,
                                         base_url) {
  # check for new datasets by entityId
  new_datasets <- get_all_manifests_out[!get_all_manifests_out$dataset_id %in% dataflow_manifest$dataset_id, ]

  # if there are new datasets...
  if (nrow(new_datasets) > 0) {

    print(paste0(nrow(new_datasets), " new dataset(s) found on Synapse"))

    # calculate number of items in each manifest
    num_items <- tryCatch(
      {
        calculate_items_per_manifest(
          df = new_datasets,
          asset_view = asset_view,
          access_token = access_token,
          na_replace = na_replace,
          base_url = base_url
        )
      },
      error = function(e) {
        message("num_items calculation failed")
        message(e)
      }
    )

    new_datasets$num_items <- as.integer(num_items)

    # fill data flow manifest rows for missing datasets
    new_datasets <- fill_dataflow_manifest(
      dataflow_manifest_chunk = new_datasets,
      schema_url = schema_url,
      na_replace = na_replace,
      base_url = base_url,
      access_token = access_token
    )

    # bind together new dataset rows and data flow manifest
    dataflow_manifest <- dplyr::bind_rows(dataflow_manifest, new_datasets)

    # rearrange data flow manifest
    dataflow_manifest <- dataflow_manifest %>%
      dplyr::group_by(source) %>%
      dplyr::arrange(source)
  }

  return(data.frame(dataflow_manifest))
}

#' Remove datasets that are no longer found in Synapse
#'
#' @param dataflow_manifest A dataFlow manifest
#' @param get_all_manifests_out The output of get_all_manifests. Also can be a dataframe that includes Component, source, entityId, dataset_name, and dataset.
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param access_token Synapse PAT
#' @param base_url Base URL of schematic API (Defaults to  AWS version)

update_manifest_remove_datasets <- function(dataflow_manifest,
                                            get_all_manifests_out,
                                            asset_view,
                                            access_token,
                                            base_url) {
  # check for removed datasets
  remove_idx <- dataflow_manifest$dataset_id %in% get_all_manifests_out$dataset_id

  # if any of the rows are flagged for removal print a message and remove from manifest
  if (any(!remove_idx)) {
    n_remove <- sum(!remove_idx)
    print(paste0(n_remove, " dataset(s) removed from Synapse"))

    dataflow_manifest <- dataflow_manifest[remove_idx, ]
  }

  return(dataflow_manifest)
}

#' Update dataFlow manifest when dataset folder name changes
#'
#' @param dataflow_manifest A dataFlow manifest
#' @param get_all_manifests_out The output of get_all_manifests. Also can be a dataframe that includes Component, source, entityId, dataset_name, and dataset.
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
  # full join two datasets and filter on mismatched updated_column
  d1 <- get_all_manifests_out[ , c("dataset_id", update_column)]
  names(d1) <- c("dataset_id", ".new_data")
  d1$.new_data[is.na(d1$.new_data)] <- ""
  df_merge <- dplyr::full_join(dataflow_manifest, d1, by = "dataset_id")
  df_merge[, update_column][is.na(df_merge[ ,update_column])] <- ""
  idx <- df_merge[ , update_column] != df_merge$.new_data

  # if any items have changed update dataset type column
  if (any(isTRUE(idx), is.na(idx))) {
    n_changed <- sum(idx)
    print(paste0("Making ", n_changed, " update(s) to ", update_column, " column"))
    dataflow_manifest <- df_merge
    dataflow_manifest[ ,update_column] <- dataflow_manifest$.new_data
    dataflow_manifest$.new_data <- NULL
    dataflow_manifest[, update_column][dataflow_manifest[ ,update_column] == ""] <- NA

    # if recalc_num_items = TRUE recalculate number of items in the manifest for updated items
    if (recalc_num_items) {
      dataflow_manifest$num_items[idx] <- calculate_items_per_manifest(
        df = dataflow_manifest[idx, ],
        asset_view = asset_view,
        access_token = access_token,
        base_url = base_url
      )
    }
  }

  # rearrange data flow manifest
  dataflow_manifest <- dataflow_manifest %>%
    dplyr::group_by(source) %>%
    dplyr::arrange(source)

  return(data.frame(dataflow_manifest))
}
