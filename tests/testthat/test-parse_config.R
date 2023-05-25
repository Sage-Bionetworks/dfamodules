test_that("get_colname_by_type", {
  tst_json <- '{"contributor": {"type": "type_1"}, "release_scheduled": {"type": "type_2"}, "entityId": {"type": "type_3"}}'

  expected_output <- c("contributor", "entityId")

  expect_equal(c(get_colname_by_type(type = "type_1", config = jsonlite::fromJSON(tst_json)),
                 get_colname_by_type(type = "type_3", config = jsonlite::fromJSON(tst_json))),
               expected_output)
})

test_that("get_renamed_colnames", {
  tst_json <- '{"contributor": {"display_name": "CONTRIBUTOR"}, "release_scheduled": {"display_name": "RELEASE DATE"}, "entityId": {"display_name": "SYNAPSE ID"}}'

  expected_output <- c("CONTRIBUTOR", "RELEASE DATE", "SYNAPSE ID")

  expect_equal(get_renamed_colnames(config = jsonlite::fromJSON(tst_json)),
               expected_output)
})

test_that("get_na_replace_colnames", {
  tst_json <- '{"contributor": {"na_replace": "Unknown"}, "release_scheduled": {"type": "type_1"}, "entityId": {"na_replace": "NO ID"}}'

  expected_output <- c("contributor", "entityId")

  expect_equal(get_na_replace_colnames(config = jsonlite::fromJSON(tst_json)),
               expected_output)
})

test_that("get_na_replace_defs", {
  tst_json <- '{"contributor": {"na_replace": "Unknown"}, "release_scheduled": {"type": "type_1"}}'
  tst_manifest <- data.frame(contributor = c("Center A", NA, "Center B"),
                             entityId = c("syn123", "syn987", NA),
                             release_scheduled = c(as.Date("2050-01-01"), as.Date("2050-01-01"), NA))


  expected_output <- list(list(targets = 1,
                               render = DT::JS(
                                 "function(data, type, row, meta) {",
                                 "return data === null ? 'Unknown' : data;",
                                 "}"
                               )))

  expect_equal(get_na_replace_defs(prepped_dataframe = tst_manifest,
                                   config = jsonlite::fromJSON(tst_json)),
               expected_output)
})
