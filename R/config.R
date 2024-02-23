##### GENERATE CONFIG #####

#' Parse config to get columns types
#'
#' @param dcc_config DCC config
#' @param na_replace Named list indicating strings to replace NA with `na_replace <- list(attribute_1 = "na string 1", attribute_2 = "na string 2")`
#' @param base_url Schematic REST API base URL
#'
#' @export

generate_dashboard_config <- function(dcc_config,
                                      base_url) {

  # check schema url
  if (httr::http_error(dcc_config$dcc$data_model_url)) {
    stop("data model URL request fails")
  }

  # GET VISUALIZE/COMPONENT
  vc_out <- visualize_component(
    dcc_config$dcc$data_model_url,
    "DataFlow",
    base_url
  )

  attributes_df <- vc_out$content

  # GET VALIDATION RULES FOR EACH ATTRIBUTE
  attributes_df$type <- unlist(sapply(attributes_df$Label, USE.NAMES = FALSE, function(lab) {
    schematic_obj <- schemas_get_node_validation_rules(
      dcc_config$dcc$data_model_url,
      lab,
      base_url
    )
    return(schematic_obj$content)
  }))

  # ADD DISPLAY NAMES / REORDER COLS
  # if null infer names from attribute_df
  if (!is.null(dcc_config$dfa_dashboard$display_names)) {
    # reorder columns based on display names
    attributes_df <- dplyr::arrange(attributes_df, match(attributes_df$Attribute, names(dcc_config$dfa_dashboard$display_names)))

    # Assign display names
    display_names <- ifelse(attributes_df$Attribute %in% names(dcc_config$dfa_dashboard$display_names), unlist(dcc_config$dfa_dashboard$display_names), NA)
  }

  attributes_df$display_name <- display_names


  # SET TYPE=ICON
  # if icon = TRUE
  if (dcc_config$dfa_dashboard$icon) {
    # find logical columns
    log_cols <- grepl("TRUE", attributes_df$`Valid Values`) & grepl("FALSE", attributes_df$`Valid Values`)

    # change type to icon
    attributes_df[log_cols, "type"] <- "icon"
  }

  # make a list
  config <- purrr::transpose(attributes_df)
  names(config) <- attributes_df$Attribute

  # return config
  return(config)
}

##### PARSE CONFIG #####

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

  # subset types (a names list) where the entry  == type
  col_names <- names(types[types == type])

  # remove NA
  col_names <- col_names[!is.na(col_names)]

  return(col_names)
}

#' Parse config to get display column names for dashboard
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' @param flatten flatten list into a vector output
#'
#' @export

get_renamed_colnames <- function(config,
                                 flatten = TRUE) {
  # create a vector of display column names
  new_col_names <- purrr::map(config, "display_name")

  if (flatten) {
    new_col_names <- purrr::flatten_chr(new_col_names)
  }

  return(new_col_names)
}

#' Parse config to get columns with na_replace specified
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#'
#' @export

get_na_replace_colnames <- function(config) {
  # create a vector of display column names
  col_names <- purrr::map(config, "na_replace")
  # remove_na
  col_names <- col_names[!is.na(col_names)]
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
    dt_replace_na(
      na_replace_idx[i],
      replacement
    )
  })

  return(defs)
}
