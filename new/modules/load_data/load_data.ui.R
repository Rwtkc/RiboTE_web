mod_load_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "page-shell ribote-page-shell ribote-page-shell--load-data",
      div(
        class = "ribote-workspace ribote-workspace--load",
        tags$aside(
          class = "ribote-sidebar",
          div(
            class = "ribote-panel ribote-panel--sidebar",
            div(class = "ribote-panel__eyebrow", "Input Setup"),
            tags$h2(class = "ribote-panel__title", "Load Data"),
            tags$p(
              class = "ribote-panel__copy",
              "Choose species context, provide the paired count matrix, and stage RNA / Ribo sample naming for downstream TE modules."
            ),
            react_control_host(
              id = ns("load_data_controls"),
              control_type = "ribote-load-data-controls",
              class = "ribote-controls-root",
              ready_input_id = ns("controls_ready")
            ),
            div(
              class = "ribote-native-file",
              fileInput(
                inputId = ns("gene_matrix"),
                label = NULL,
                multiple = FALSE,
                accept = c(".csv", ".txt", ".tsv", ".xlsx")
              )
            )
          )
        ),
        tags$section(
          class = "ribote-main",
          div(
            class = "ribote-panel ribote-panel--intro",
            div(class = "ribote-panel__eyebrow", "Session Summary"),
            uiOutput(ns("session_summary"))
          )
        )
      )
    )
  )
}
