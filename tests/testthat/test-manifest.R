schema_url <- "https://raw.githubusercontent.com/Sage-Bionetworks/dfamodules/fix-test/tests/testthat/test_data/data_model/dataflow_component.jsonld"
schematic_api_url <- "https://schematic.api.sagebionetworks.org"

# read in test config
config <- generate_dashboard_config(schema_url = schema_url,
                                    display_names = list(contributor = "Contributor",
                                                         entityId = "Synapse ID",
                                                         dataset = "Data Type",
                                                         dataset_name = "Dataset Folder Name",
                                                         num_items = "Number of Items in Manifest",
                                                         status = "Data Flow Status",
                                                         release_scheduled = "Release Date",
                                                         embargo = "Embargo",
                                                         standard_compliance = "QC Checks",
                                                         released = "Released",
                                                         data_portal = "Data Portal",
                                                         Component = NA),
                                    icon = TRUE,
                                    na_replace = list(num_items = "No Manifest",
                                                      release_scheduled = "Not Scheduled",
                                                      embargo = "No Embargo",
                                                      dataset = "No Manifest"),
                                    add_filter = c("num_items", "data_portal"),
                                    base_url = schematic_api_url)

# make mock synapse / submission ready manifest
manifest_synapse <- data.frame(Component = rep("DataFlow", 4),
                               contributor = rep("schematic - main", 4),
                               entityId = paste0("syn", 1:4),
                               dataset_name = paste0("dataset_name", 1:4),
                               dataset = rep("Biospecimen", 4),
                               num_items = c(rep("Not Applicable", 3), "1"),
                               status = c("not_scheduled", "not_scheduled", "quarantine", "quarantine"),
                               release_scheduled = c(rep("Not Applicable", 2),
                                                     rep("2050-01-01", 2)),
                               embargo = c(rep("Not Applicable", 2),
                                           rep("2050-01-01", 2)),
                               standard_compliance = rep(FALSE, 4),
                               data_portal = rep(FALSE, 4),
                               released = rep(FALSE, 4))

# make mock dfa ready manifest
manifest_dfa <- data.frame(Component = rep("DataFlow", 4),
                           contributor = rep("schematic - main", 4),
                           entityId = paste0("syn", 1:4),
                           dataset_name = paste0("dataset_name", 1:4),
                           dataset = rep("Biospecimen", 4),
                           num_items = c(rep(NA, 3), 1),
                           status = c("not_scheduled", "not_scheduled", "quarantine", "quarantine"),
                           release_scheduled = c(rep(as.Date(NA), 2),
                                                 rep(as.Date("2050-01-01"), 2)),
                           embargo = c(rep(as.Date(NA), 2),
                                       rep(as.Date("2050-01-01"), 2)),
                           standard_compliance = rep(FALSE, 4),
                           data_portal = rep(FALSE, 4),
                           released = rep(FALSE, 4))

# tests

# test_that("update_dfs_manifest", {
#
#   dfs_updates <-  list(release_scheduled = as.Date("2022-01-01"),
#                        embargo = as.Date("2022-01-01"),
#                        standard_compliance = TRUE,
#                        data_portal = TRUE,
#                        released = TRUE)
#
#   selected_datasets_df <- data.frame(id = c("syn1"), name = "dataset_name1")
#
#   expected_updated_row <- data.frame(Component = "DataFlow",
#                                      contributor = as.factor("schematic - main"),
#                                      entityId = "syn1",
#                                      dataset_name = paste0("dataset_name", 1),
#                                      dataset = as.factor("Biospecimen"),
#                                      num_items = as.numeric(NA),
#                                      release_scheduled = as.Date("2022-01-01"),
#                                      embargo = as.Date("2022-01-01"),
#                                      standard_compliance = TRUE,
#                                      data_portal = TRUE,
#                                      released = TRUE)
#   expected_df <- rbind(expected_updated_row, manifest_dfa[-1,])
#
#   expect_equal(update_dfs_manifest(dfs_manifest = manifest_dfa,
#                                    dfs_updates = dfs_updates,
#                                    selected_datasets_df = selected_datasets_df),
#                expected_df)
# })

test_that("prep_manifest_dfa", {
  expect_equal(prep_manifest_dfa(manifest_synapse, config),
               manifest_dfa)
})

test_that("prep_manifest_submit", {
  expect_equal(prep_manifest_submit(manifest_dfa, config),
               manifest_synapse)
})
