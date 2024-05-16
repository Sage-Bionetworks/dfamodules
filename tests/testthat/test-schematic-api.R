# VARIABLES #############################################################################
# DFA TEST
asset_view <- "syn55226012"
project_id <- "syn55226002"
dataset_id <- "syn59424225"
access_token <- Sys.getenv("SYNAPSE_PAT")
base_url <- Sys.getenv("DFA_SCHEMATIC_API_URL")
schema_url <- "https://raw.githubusercontent.com/Sage-Bionetworks/data-models/main/example.model.jsonld"

# if running interactively, run load_all()
# https://r-pkgs.org/misc.html#sec-misc-inst
if ( interactive() ) {
  devtools::load_all()
}

pass_csv_path <- system.file(
  "test_data/biospecimen_pass.csv",
  package = "dfamodules"
  )

# TEST API ##############################################################################

test_that("storage_projects successfully returns a schematic_api object", {
  sp <- try(
    storage_projects(
      asset_view = asset_view,
      access_token = access_token,
      base_url = base_url
    ),
    silent = FALSE
  )

  expect_true(class(sp) == "schematic_api")
})

test_that("storage_project_datasets successfully returns a schematic_api object", {
  spd <- try(
    storage_project_datasets(
      asset_view = asset_view,
      project_id = project_id,
      access_token = access_token,
      base_url = base_url
    ),
    silent = FALSE
  )

  expect_true(class(spd) == "schematic_api")
})

test_that("manifest_download successfully returns a schematic_api object", {
  md <- try(
    dataset_manifest_download(
      access_token = access_token,
      asset_view = asset_view,
      dataset_id = dataset_id,
      base_url = base_url
    ),
    silent = FALSE
  )

  expect_true(class(md) == "schematic_api")
})

test_that("model_submit successfully returns a schematic_api object", {
  s <- try(
    model_submit(
      data_type = "Biospecimen",
      asset_view = asset_view,
      dataset_id = dataset_id,
      file_name = pass_csv_path,
      access_token = access_token,
      restrict_rules = FALSE,
      manifest_record_type = "file_only",
      base_url = base_url,
      schema_url = schema_url,
      use_schema_label = TRUE
    ),
    silent = FALSE
  )

  expect_true(class(s) == "schematic_api")
})

test_that("storage_project_manifests successfully returns a schematic_api object", {
  spm <- try(
    storage_project_manifests(
      asset_view,
      project_id,
      access_token,
      base_url
    ),
    silent = FALSE
  )

  expect_true(class(spm) == "schematic_api")
})

test_that("visualize_component returns a schematic_api object", {
  vc <- try(
    visualize_component(
      schema_url = schema_url,
      component = "Biospecimen",
      base_url = base_url
    ),
    silent = FALSE
  )

  expect_true(class(vc) == "schematic_api")
})

test_that("schemas_get_node_validation_rules returns a schematic_api object", {
  spm <- try(
    schemas_get_node_validation_rules(
      schema_url = schema_url,
      node_display_name = "TissueStatus",
      base_url
    ),
    silent = FALSE
  )

  expect_true(class(spm) == "schematic_api")
})
