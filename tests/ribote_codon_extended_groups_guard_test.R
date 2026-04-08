codon_shared_code <- paste(
  readLines("new/modules/codon/codon.shared.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_server_code <- paste(
  readLines("new/modules/codon/codon.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_ui_code <- paste(
  readLines("new/modules/codon/codon.ui.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/CodonResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_chart_code <- paste(
  readLines("new/frontend/app_shell/src/components/codonChart.js", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
shift_enrichment_chart_code <- sub(
  "(?s)^.*?(export function drawCodonShiftEnrichmentChart)",
  "\\1",
  codon_chart_code,
  perl = TRUE
)
shift_enrichment_chart_code <- sub(
  "(?s)export function drawCodonPermutationHistogramChart.*$",
  "",
  shift_enrichment_chart_code,
  perl = TRUE
)

required_views <- c(
  "selected_codon_burden",
  "codon_enrichment_shifted",
  "selected_codon_across_groups",
  "permutation_support",
  "te_bias_selected_load",
  "selected_load_effect",
  "codon_clustering",
  "codon_usage_heatmap",
  "codon_run_zscore",
  "codon_run_enrichment"
)

required_result_keys <- c(
  "selectedCodonBurden",
  "codonEnrichmentShifted",
  "selectedCodonAcrossGroups",
  "permutationSupport",
  "teBiasSelectedLoad",
  "selectedLoadEffect",
  "codonClustering",
  "codonUsageHeatmap",
  "codonRunZscore",
  "codonRunEnrichment"
)

for (view_id in required_views) {
  stopifnot(grepl(sprintf('"%s"', view_id), codon_shared_code, fixed = TRUE))
}

stopifnot(grepl('inputId = ns\\("run_te_shift_enrichment"\\)', codon_ui_code))
stopifnot(grepl('inputId = ns\\("run_pattern_views"\\)', codon_ui_code))
stopifnot(grepl('inputId = ns\\("run_codon_runs"\\)', codon_ui_code))
stopifnot(grepl('uiOutput\\(ns\\("run_te_shift_enrichment_hint"\\)\\)', codon_ui_code))
stopifnot(grepl('uiOutput\\(ns\\("run_pattern_views_hint"\\)\\)', codon_ui_code))
stopifnot(grepl('uiOutput\\(ns\\("run_codon_runs_hint"\\)\\)', codon_ui_code))
stopifnot(!grepl('run_group_placeholder_shift', codon_ui_code, fixed = TRUE))
stopifnot(!grepl('run_group_placeholder_pattern', codon_ui_code, fixed = TRUE))
stopifnot(!grepl('run_group_placeholder_runs', codon_ui_code, fixed = TRUE))

stopifnot(grepl('te_shift_context <- reactiveVal\\(NULL\\)', codon_server_code))
stopifnot(grepl('pattern_views_context <- reactiveVal\\(NULL\\)', codon_server_code))
stopifnot(grepl('codon_runs_context <- reactiveVal\\(NULL\\)', codon_server_code))
stopifnot(grepl('observeEvent\\(input\\$run_te_shift_enrichment, \\{', codon_server_code))
stopifnot(grepl('observeEvent\\(input\\$run_pattern_views, \\{', codon_server_code))
stopifnot(grepl('observeEvent\\(input\\$run_codon_runs, \\{', codon_server_code))

for (result_key in required_result_keys) {
  stopifnot(grepl(sprintf("%s = list\\(", result_key), codon_server_code))
}

for (view_id in required_views) {
  stopifnot(grepl(sprintf('currentView?.id === "%s"', view_id), codon_results_code, fixed = TRUE))
}

stopifnot(grepl("includeZero", codon_chart_code, fixed = TRUE))
stopifnot(grepl("includeZero: true", codon_chart_code, fixed = TRUE))
stopifnot(!grepl("symmetricAroundZero", codon_chart_code, fixed = TRUE))
stopifnot(!grepl("zero baseline", shift_enrichment_chart_code, fixed = TRUE))
stopifnot(!grepl("TE Down enriched values fall left of zero", shift_enrichment_chart_code, fixed = TRUE))
stopifnot(grepl('.append("rect")', shift_enrichment_chart_code, fixed = TRUE))
stopifnot(!grepl('.append("circle")', shift_enrichment_chart_code, fixed = TRUE))

cat("ribote codon extended groups guard test passed\n")
