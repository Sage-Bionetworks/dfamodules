# read in test config
dcc_config <- readr::read_csv("https://raw.githubusercontent.com/Sage-Bionetworks/data_flow_config/main/dcc_config.csv",
                              show_col_types = FALSE)
dcc_config <- dcc_config[grepl("Fair Demo", dcc_config$project_name), ]

# Download Manifest ---------
manifest <- dataset_manifest_download(
  asset_view = dcc_config$synapse_asset_view,
  dataset_id = dcc_config$manifest_dataset_id,
  access_token = Sys.getenv("SYNAPSE_PAT"),
  base_url = Sys.getenv("DFA_SCHEMATIC_API_URL")
)

manifest <- manifest$content

# Generate Dash ---------
config <- generate_dashboard_config(
  schema_url = dcc_config$schema_url,
  base_url = Sys.getenv("DFA_SCHEMATIC_API_URL")
  )

# Run tests ---------

dfa_manifest <- prep_manifest_dfa(manifest, config)

test_that("prep_manifest_dfa updates date attributes correctly", {
  dfa_manifest <- prep_manifest_dfa(manifest, config)
  manifest_classes <- sapply(dfa_manifest, class)
  config_date <- get_colname_by_type("date", config)
  manifest_date <- names(manifest_classes[manifest_classes == "Date"])

  expect_equal(
    sort(manifest_date),
    sort(config_date)
  )

})

test_that("prep_manifest_dfa updates int attributes correctly", {
  manifest_classes <- sapply(dfa_manifest, class)
  config_int <- get_colname_by_type("int", config)
  manifest_int <- names(manifest_classes[manifest_classes == "integer"])

  expect_equal(
    sort(manifest_int),
    sort(config_int)
    )
})

test_that("prep_manifest_submit replaces NA with ''", {
  submit_manifest <- prep_manifest_submit(
    dfa_manifest,
    config,
    na_replace = ""
    )

  expect_false(any(is.na(submit_manifest)))
})

test_that("prep_manifest_submit replaces changes date columns back to character", {
  submit_manifest <- prep_manifest_submit(
    dfa_manifest,
    config,
    na_replace = ""
  )

  expect_true(class(submit_manifest$release_date) == "character")
})
