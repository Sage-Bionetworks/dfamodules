#########################################################
## Functions that create stylized datatable dashboards ##
#########################################################

#' Create a dashboard style datatable
#'
#' @param df A dataframe prepared by `prep_df_for_dash()` with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param config Config for datatable dashboard module in `inst/datatable_dashboard_config.json`
#'
#' @export
#'

create_dashboard <- function(df,
                             config) {

  prepped_df <- prep_manifest_dash(df,
                                   config)

  style_dashboard(prepped_df, config)
}

#' Add custom styling to dashboard based on contents of config
#'
#' @param prepped_manifest A manifest prepped with `prep_manifest_dash()`
#' @param config `inst/datatable_dashboard_config.json`
#'
#' @export
#'

style_dashboard <- function(prepped_manifest,
                            config) {

  # get icon col index
  icon_idx <- match(get_colname_by_type("icon", config), names(prepped_manifest))

  # define center styling for icon columns
  center_list <- list(targets = icon_idx, className = 'dt-center')

  # hide columns where display_name = NA and that are not in config
  display_names <- purrr::map_chr(config, "display_name")
  hide_cols <- c(names(display_names[is.na(display_names)]),
                 setdiff(names(prepped_manifest), names(config)))
  hide_idx <- match(hide_cols, names(prepped_manifest))
  hide_list <- list(targets = hide_idx, visible = FALSE)

  defs <- list(center_list,
               hide_list)

  # define styling for na_replacement
  na_replace_defs <- get_na_replace_defs(prepped_manifest,
                                         config)

  defs <- append(defs, na_replace_defs)

  # get column names for datatable display
  colnames <- get_renamed_colnames(config)
  # put empty string in front to account for rownum column
  colnames <- c("", colnames)

  # create datatable
  dt <- DT::datatable(prepped_manifest,
                      escape = FALSE,
                      selection = "none",
                      filter = "none",
                      colnames = colnames,
                      options = list(scrollX = TRUE,
                                     scrollY = 500,
                                     bPaginate = FALSE,
                                     searching = FALSE,
                                     columnDefs = defs))

  dt
}

#' Prepare a dataframe for a dashboard style datatable
#'
#' @param df A dataframe with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param config Config for datatable dashboard module in `inst/datatable_dashboard_config.json`
#'
#' @export
#'

prep_manifest_dash <- function(df,
                               config) {

  # convert TRUE / FALSE to icon html

  df <- convert_column_type(df = df,
                            col_names = get_colname_by_type(config, type = "icon"),
                            type = "icon")

  # convert certain columns to factors
  # enables drop down selection style filtering for column
  # df <- convert_column_type(df = df,
  #                           col_names = get_colname_by_type(config, type = "drop_down_filter"),
  #                           type = "factor")

  # rearrange dataframe based on config order (any columns not in config are moved to end of dataframe)
  expected_colnames <- names(config)
  df <- rearrange_dataframe(df, expected_colnames)

  return(df)
}


## HELPERS ##############################################################################

#' NA replacement - datatable custom JS
#'
#' @param col_index target columns index
#' @param na_replacement text to replace NA
#'
#' @importFrom glue glue
#' @export
#'

dt_replace_na <- function(col_index,
                          na_replacement) {

  list(targets = col_index,
       render = DT::JS(
         "function(data, type, row, meta) {",
         glue::glue("return data === null ? '{na_replacement}' : data;"),
         "}"
         ))
}
