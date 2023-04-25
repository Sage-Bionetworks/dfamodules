test_that("list_to_dataframe", {
  mock_storage_projects_output <- list(list("syn123", "project 1"),
                                       list("syn345", "project 2"))

  expected_output <- data.frame(X1 = c("syn123", "syn345"),
                                X2 = c("project 1", "project 2"))

  expect_equal(list_to_dataframe(mock_storage_projects_output),
               expected_output)
})

test_that("convert_column_type", {
  testing_df <- data.frame(int = "1", factor = "dog", logic = "TRUE", char = 1, date = "2020-01-01")

  testing_df <- convert_column_type(df = testing_df,
                                    col_names = "factor",
                                    type = "factor")

  testing_df <- convert_column_type(df = testing_df,
                                    col_names = "int",
                                    type = "integer")

  testing_df <- convert_column_type(df = testing_df,
                                    col_names = "date",
                                    type = "date")

  testing_df <- convert_column_type(df = testing_df,
                                    col_names = "char",
                                    type = "character")

  testing_df <- convert_column_type(df = testing_df,
                                    col_names = "logic",
                                    type = "logical")

  expected_output <- data.frame(int = as.integer(1),
                                factor = as.factor("dog"),
                                logic = TRUE,
                                char = "1",
                                date = as.Date("2020-01-01"))

  expect_identical(testing_df,
                   expected_output)
})

test_that("true_false_icon", {
  testing_vec <- c("TRUE", "FALSE")

  expected_output <- c(as.character(shiny::icon("check", lib = "font-awesome")),
                       as.character(shiny::icon("xmark", lib = "font-awesome")))

  expect_equal(true_false_icon(testing_vec),
               expected_output)
})

test_that("rearrange_dataframe", {
  testing_df <- data.frame(a = c(1,2,3), b = c(1,2,3), c = c(1,2,3), d = c(1,2,3))

  expected_output <- data.frame(b = c(1,2,3), a = c(1,2,3), d = c(1,2,3),  c = c(1,2,3))


  expect_equal(rearrange_dataframe(testing_df,
                                   expected_col_names = c("b", "a", "d")),
               expected_output)
})
