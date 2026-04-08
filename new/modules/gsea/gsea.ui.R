mod_gsea_ui <- function(id) {
  ns <- NS(id)
  config <- gsea_module_config()

  div(
    class = paste("page-shell ribote-page-shell", paste0("ribote-page-shell--", config$key)),
    div(
      class = "ribote-workspace ribote-workspace--wide-sidebar",
      tags$aside(
        class = "ribote-sidebar",
        div(
          class = "ribote-panel ribote-panel--sidebar",
          div(class = "ribote-panel__eyebrow", config$eyebrow),
          tags$h2(class = "ribote-panel__title", config$title),
          tags$p(class = "ribote-panel__copy", config$description),
          div(
            class = "ribote-sidebar__action",
            actionButton(
              inputId = ns("run_analysis"),
              label = config$run_label,
              class = "ribote-btn ribote-btn--primary",
              `data-rnameta-analysis-trigger` = "true",
              `data-rnameta-analysis-owner` = ns("progress_slot"),
              `data-rnameta-analysis-snapshot` = "true"
            )
          ),
          uiOutput(ns("run_hint")),
          div(
            class = "ribote-sidebar__export",
            react_control_host(
              id = ns("export_host"),
              control_type = "ribote-gsea-export",
              class = "ribote-export-root",
              ready_input_id = ns("export_ready")
            )
          ),
          div(
            class = "ribote-controls",
            react_control_host(
              id = ns("controls_host"),
              control_type = "ribote-module-controls",
              class = "ribote-controls-root",
              ready_input_id = ns("controls_ready")
            )
          )
        )
      ),
      tags$section(
        class = "ribote-main",
        div(
          class = "ribote-panel ribote-panel--intro",
          div(class = "ribote-panel__eyebrow", "Module Snapshot"),
          tags$h3(class = "ribote-main__title", paste(config$title, "Workspace")),
          tags$p(
            class = "ribote-main__copy",
            "Interpret Translation Efficiency changes by asking whether coordinated pathways or biological processes are enriched across the ranked gene list."
          ),
          uiOutput(ns("parameter_snapshot"))
        ),
        div(
          class = "ribote-panel ribote-panel--canvas",
          div(class = "ribote-panel__eyebrow", "Analysis Flow"),
          div(
            class = "ribote-progress-slot",
            `data-rnameta-progress-slot` = ns("progress_slot")
          ),
          uiOutput(ns("analysis_summary")),
          tags$div(
            class = "ribote-gsea-results-slot",
            react_control_host(
              id = ns("results_host"),
              control_type = "ribote-gsea-results",
              class = "ribote-results-root",
              ready_input_id = ns("results_ready")
            )
          )
        )
      )
    )
  )
}
