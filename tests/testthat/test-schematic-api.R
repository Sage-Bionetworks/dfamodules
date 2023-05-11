# adapted from afwillia's work on data_curator
# (https://github.com/Sage-Bionetworks/data_curator/blob/schematic-rest-api/tests/testthat/test_schematic_rest_api.R)

# VARIABLES #############################################################################
# FAIR DEMO DATA PROJECT A
asset_view <- "syn50896957"
project_id <- "syn50896931"
dataset_id <- "syn51219090"
access_token <- Sys.getenv("SYNAPSE_PAT")
base_url <- Sys.getenv("SCHEMATIC_BASE_URL_AWS")
testing_manifest_path <- "test_data/synapse_storage_manifest_dataflow.csv"
schema_url <- "https://raw.githubusercontent.com/Sage-Bionetworks/data_flow/main/inst/data_flow_component.jsonld"

# TEST API ##############################################################################

test_that("storage_projects successfully returns a schematic_api object", {
  sp <- try(storage_projects(asset_view = asset_view,
                             access_token = access_token,
                             base_url = base_url),
            silent = FALSE)

  expect_true(class(sp) == "schematic_api")
})

test_that("storage_project_datasets successfully returns a schematic_api object", {
  spd <- try(storage_project_datasets(asset_view = asset_view,
                                      project_id = project_id,
                                      access_token = access_token,
                                      base_url = base_url),
             silent = FALSE)

  expect_true(class(spd) == "schematic_api")
})

test_that("manifest_download successfully returns a schematic_api object", {
  md <- try(dataset_manifest_download(access_token = access_token,
                              asset_view = asset_view,
                              dataset_id = dataset_id,
                              base_url = base_url),
            silent = FALSE)

  expect_true(class(md) == "schematic_api")

})

test_that("model_submit successfully returns a schematic_api object", {
  s <- try(model_submit(data_type = NULL,
                        asset_view = asset_view,
                        dataset_id = dataset_id,
                        file_name = testing_manifest_path,
                        access_token = access_token,
                        restrict_rules = TRUE,
                        manifest_record_type = "table_and_file",
                        base_url = base_url,
                        schema_url = schema_url,
                        use_schema_label = TRUE),
           silent = FALSE)

  expect_true(class(s) == "schematic_api")
})

test_that("storage_project_manifests successfully returns a schematic_api object", {
  spm <- try(storage_project_manifests(asset_view,
                                      project_id,
                                      access_token,
                                      base_url),
            silent = FALSE)

  expect_true(class(spm) == "schematic_api")

})
