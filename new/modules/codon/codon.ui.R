mod_codon_ui <- function(id) {
  ns <- NS(id)
  config <- codon_module_config()
  input_profile <- codon_sidebar_profile("Input and Usage")
  bias_profile <- codon_sidebar_profile("Codon Bias")
  shift_profile <- codon_sidebar_profile("TE Shift and Enrichment")
  pattern_profile <- codon_sidebar_profile("Pattern Views")
  run_profile <- codon_sidebar_profile("Codon Runs")

  div(
    class = paste("page-shell ribote-page-shell", paste0("ribote-page-shell--", config$key)),
    div(
      class = "ribote-workspace ribote-workspace--wide-sidebar",
      tags$aside(
        class = "ribote-sidebar",
        div(
          class = "ribote-panel ribote-panel--sidebar",
          div(
            id = ns("sidebar_group_host"),
            class = "ribote-codon-sidebar-shell",
            `data-ribote-codon-active-group` = "Input and Usage",
            div(
              class = "ribote-codon-sidebar-panel",
              `data-ribote-codon-group` = "Input and Usage",
              div(class = "ribote-panel__eyebrow", input_profile$eyebrow),
              tags$h2(class = "ribote-panel__title", input_profile$title),
              tags$p(class = "ribote-panel__copy", input_profile$description),
              div(
                class = "ribote-sidebar__action",
                actionButton(
                  inputId = ns("run_input_usage"),
                  label = config$run_label,
                  class = "ribote-btn ribote-btn--primary ribote-btn--block",
                  `data-rnameta-analysis-trigger` = "true",
                  `data-rnameta-analysis-owner` = ns("progress_slot")
                )
              ),
              uiOutput(ns("run_input_usage_hint")),
              div(
                class = "ribote-controls",
                react_control_host(
                  id = ns("controls_host"),
                  control_type = "ribote-module-controls",
                  class = "ribote-controls-root",
                  ready_input_id = ns("controls_ready")
                )
              )
            ),
            div(
              class = "ribote-codon-sidebar-panel",
              `data-ribote-codon-group` = "Codon Bias",
              div(class = "ribote-panel__eyebrow", bias_profile$eyebrow),
              tags$h2(class = "ribote-panel__title", bias_profile$title),
              tags$p(class = "ribote-panel__copy", bias_profile$description),
              div(
                class = "ribote-sidebar__action",
                actionButton(
                  inputId = ns("run_codon_bias"),
                  label = "Run Codon Bias",
                  class = "ribote-btn ribote-btn--primary ribote-btn--block",
                  `data-rnameta-analysis-trigger` = "true",
                  `data-rnameta-analysis-owner` = ns("progress_slot")
                )
              ),
              uiOutput(ns("run_codon_bias_hint")),
              div(
                class = "ribote-result-card",
                tags$p(class = "ribote-result-card__copy", bias_profile$note)
              )
            ),
            div(
              class = "ribote-codon-sidebar-panel",
              `data-ribote-codon-group` = "TE Shift and Enrichment",
              div(class = "ribote-panel__eyebrow", shift_profile$eyebrow),
              tags$h2(class = "ribote-panel__title", shift_profile$title),
              tags$p(class = "ribote-panel__copy", shift_profile$description),
              div(
                class = "ribote-sidebar__action",
                actionButton(
                  inputId = ns("run_te_shift_enrichment"),
                  label = "Run TE Shift and Enrichment",
                  class = "ribote-btn ribote-btn--primary ribote-btn--block",
                  `data-rnameta-analysis-trigger` = "true",
                  `data-rnameta-analysis-owner` = ns("progress_slot")
                )
              ),
              uiOutput(ns("run_te_shift_enrichment_hint")),
              div(
                class = "ribote-result-card",
                tags$p(class = "ribote-result-card__copy", shift_profile$note)
              )
            ),
            div(
              class = "ribote-codon-sidebar-panel",
              `data-ribote-codon-group` = "Pattern Views",
              div(class = "ribote-panel__eyebrow", pattern_profile$eyebrow),
              tags$h2(class = "ribote-panel__title", pattern_profile$title),
              tags$p(class = "ribote-panel__copy", pattern_profile$description),
              div(
                class = "ribote-sidebar__action",
                actionButton(
                  inputId = ns("run_pattern_views"),
                  label = "Run Pattern Views",
                  class = "ribote-btn ribote-btn--primary ribote-btn--block",
                  `data-rnameta-analysis-trigger` = "true",
                  `data-rnameta-analysis-owner` = ns("progress_slot")
                )
              ),
              uiOutput(ns("run_pattern_views_hint")),
              div(
                class = "ribote-result-card",
                tags$p(class = "ribote-result-card__copy", pattern_profile$note)
              )
            ),
            div(
              class = "ribote-codon-sidebar-panel",
              `data-ribote-codon-group` = "Codon Runs",
              div(class = "ribote-panel__eyebrow", run_profile$eyebrow),
              tags$h2(class = "ribote-panel__title", run_profile$title),
              tags$p(class = "ribote-panel__copy", run_profile$description),
              div(
                class = "ribote-sidebar__action",
                actionButton(
                  inputId = ns("run_codon_runs"),
                  label = "Run Codon Runs",
                  class = "ribote-btn ribote-btn--primary ribote-btn--block",
                  `data-rnameta-analysis-trigger` = "true",
                  `data-rnameta-analysis-owner` = ns("progress_slot")
                )
              ),
              uiOutput(ns("run_codon_runs_hint")),
              div(
                class = "ribote-result-card",
                tags$p(class = "ribote-result-card__copy", run_profile$note)
              )
            )
          ),
          div(
            class = "ribote-sidebar__export",
            react_control_host(
              id = ns("export_host"),
              control_type = "ribote-codon-export",
              class = "ribote-export-root",
              ready_input_id = ns("export_ready")
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
            "Assess selected codon usage across Translation Efficiency groups, then connect transcript-level codon bias and adaptation features to the same TE workspace."
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
          div(
            id = ns("analysis_summary_shell"),
            class = "ribote-codon-analysis-summary-shell",
            `data-ribote-codon-active-group` = "Input and Usage",
            uiOutput(ns("analysis_summary"))
          ),
          tags$div(
            class = "ribote-codon-results-slot",
            react_control_host(
              id = ns("results_host"),
              control_type = "ribote-codon-results",
              class = "ribote-results-root",
              ready_input_id = ns("results_ready")
            )
          )
        )
      )
    )
  )
}
