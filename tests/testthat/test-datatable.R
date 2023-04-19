test_that("dt_replace_na", {
  
  expected_output <- list(targets = 1, 
                          render = DT::JS(
                            "function(data, type, row, meta) {",
                            "return data === null ? 'Not Applicable' : data;",
                            "}"
                          ))
  
  expect_equal(dt_replace_na(col_index = 1,
                             na_replacement = "Not Applicable"),
               expected_output)
})

# FIXME how to test DT::datatable output?