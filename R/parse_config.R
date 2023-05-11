#' Parse config to get columns types
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' @param type column type as described in datatable_dashboard_config.json
#'
#' @export

get_colname_by_type <- function(type,
                                config) {

  # get all elements with 'type'
  type_list <- purrr::map(config, "type")
  types <- unlist(type_list)

  #subset types (a names list) where the entry  == type
  col_names <- names(types[types == type])

  # remove NA
  col_names <- col_names[!is.na(col_names)]

  return(col_names)

}

#' Parse config to get display column names for dashboard
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#'
#' @export

get_renamed_colnames <- function(config) {
  # create a vector of display column names
  new_col_names <- purrr::map(config, "col_name")

  purrr::flatten_chr(new_col_names)
}

#' Parse config to get columns with na_replace specified
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#'
#' @export

get_na_replace_colnames <- function(config) {
  # create a vector of display column names
  col_names <- purrr::map(config, "na_replace")
  names(purrr::flatten(col_names))
}

#' Parse config to get na replacement definitions with custom javascript. Outputs a list.
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' @param prepped_dataframe dataframe output by `prep_df_for_dash()`
#'
#' @export

get_na_replace_defs <- function(prepped_dataframe,
                                config) {

  # get na_replace columns
  na_replace_cols <- get_na_replace_colnames(config)

  # get colname index in prepped dataframe
  na_replace_idx <- match(na_replace_cols, names(prepped_dataframe))

  defs <- lapply(seq_along(na_replace_cols), function(i) {
    colname <- na_replace_cols[i]
    replacement <- config[[colname]]$na_replace
    dt_replace_na(na_replace_idx[i],
                  replacement)
  })

  return(defs)
}
