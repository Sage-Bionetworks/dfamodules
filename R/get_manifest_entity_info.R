#' Get synapse entity information of datasets in a manifest
#' 
#' @param manifest A data.frame with the column `entity_id`
#' @param access_token A Synapse Personal Access Token
get_manifest_entity_info <- function(manifest, access_token) {
  if (!inherits(manifest, "data.frame")) stop(sprintf("manifest must be a data.frame"))
  if (!"dataset_id" %in% names(manifest)) stop("manifest must have a column `dataset_id`")
  file_list <- lapply(manifest$dataset_id, function(x) {
    entities <- dfamodules::synapse_entity_children(
      auth = access_token,
      parentId = x,
      includeTypes = list("file"))
    entities$dataset_id <- x
    entities
  })
  files <- dplyr::bind_rows(file_list)
  if (nrow(files)) {
    files <- dplyr::filter(files, grepl("synapse_storage_manifest", name))
    files <- dplyr::select(files, modified_on=modifiedOn, dataset_id)
    if ("modified_on" %in% names(manifest)) {
      manifest <- dplyr::select(manifest, -modified_on)
    }
    manifest <- merge(
      x = manifest,
      y = files,
      by = "dataset_id",
      all.x = TRUE
    )
  }
  manifest
}
