#' submit_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#' @importFrom shiny NS tagList
#' @export

mod_submit_model_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    waiter::useWaiter(),
    shiny::actionButton(ns("submit"), "Submit to Synapse")
  )
}

#' submit_model Server Functions
#'
#' @param id shiny id
#' @param dfs_manifest a manifest
#' @param data_type manifest component type
#' @param asset_view an ID of a file view listing all project data assets
#' @param dataset_id the ID of the dataset that contains manifest
#' @param manifest_dir directory where manifest is stored
#' @param access_token a Synapse Personal Access Token
#' @param schema_url A data model URL
#' @param base_url a Schematic API base URL. Default is
#' `https://schematic.api.sagebionetworks.org`
#'
#' @export

mod_submit_model_server <- function(id,
                                    dfs_manifest,
                                    data_type,
                                    asset_view,
                                    dataset_id,
                                    manifest_dir = "./manifest",
                                    access_token,
                                    schema_url,
                                    base_url = paste0(
                                      "https://",
                                      "schematic.api.sagebionetworks",
                                      ".org"
                                    )) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # FIXME: Stop writing csv, use json instead
    # Blocked due to schematic api limitations

    # create manifest directory if it doesn't exist yet
    if (!file.exists(manifest_dir)) {
      dir.create(manifest_dir)
    }

    # on button click submit model to synapse
    shiny::observeEvent(input$submit, {
      # write manifest csv
      path <- file.path(manifest_dir, "synapse_storage_manifest_dataflow.csv")
      utils::write.table(dfs_manifest(),
        path,
        sep = ",",
        row.names = FALSE
      )

      waiter::waiter_show(html = htmltools::div(
        style = "color:#424874;",
        waiter::spin_3(),
        htmltools::h4("Submitting updated manifest to Synapse...")
      ))

      # submit model to synapse
      model_submit(
        data_type = data_type,
        asset_view = asset_view(),
        dataset_id = dataset_id(),
        file_name = path,
        restrict_rules = TRUE,
        access_token = access_token,
        manifest_record_type = "file_only",
        schema_url = schema_url(),
        base_url = base_url
      )

      waiter::waiter_hide()
    })
  })
}
