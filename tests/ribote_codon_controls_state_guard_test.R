codon_server_code <- paste(
  readLines("new/modules/codon/codon.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

module_controls_code <- paste(
  readLines("new/frontend/app_shell/src/components/ModuleControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_ui_code <- paste(
  readLines("new/modules/codon/codon.ui.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("active_group <- reactiveVal\\(\"Input and Usage\"\\)", codon_server_code))
stopifnot(grepl("results_publish_token <- 0L", codon_server_code))
stopifnot(grepl("active_sections <- codon_sections_for_group\\(\"Input and Usage\"\\)", codon_server_code))
stopifnot(grepl("title = config\\$title", codon_server_code))
stopifnot(grepl("previous_group <- active_group\\(\\)", codon_server_code))
stopifnot(grepl("next_group <- codon_view_group\\(next_view\\)", codon_server_code))
stopifnot(grepl("latest_result_view_request_seq <- reactiveVal\\(0\\)", codon_server_code))
stopifnot(grepl("seq_advanced <- FALSE", codon_server_code, fixed = TRUE))
stopifnot(grepl("if \\(is.finite\\(request_seq\\)\\) \\{[[:space:]]*if \\(request_seq < latest_result_view_request_seq\\(\\)\\) \\{[[:space:]]*return\\(invisible\\(NULL\\)\\)", codon_server_code))
stopifnot(grepl("seq_advanced <- request_seq > latest_result_view_request_seq\\(\\)", codon_server_code))
stopifnot(grepl("if \\(!identical\\(next_group, previous_group\\)\\) \\{[[:space:]]*active_group\\(next_group\\)[[:space:]]*\\}", codon_server_code))
stopifnot(!grepl("if \\(!identical\\(next_group, previous_group\\)\\) \\{[[:space:]]*active_group\\(next_group\\)[[:space:]]*publish_controls\\(\\)[[:space:]]*\\}", codon_server_code))
stopifnot(grepl("\\} else if \\(seq_advanced\\) \\{[[:space:]]*publish_results\\(\\)[[:space:]]*publish_export\\(\\)[[:space:]]*\\}", codon_server_code))
stopifnot(grepl("results_publish_token <<- results_publish_token \\+ 1L", codon_server_code))
stopifnot(grepl("activeViewToken = results_publish_token", codon_server_code, fixed = TRUE))
stopifnot(grepl("summaryHostId = session\\$ns\\(\"analysis_summary_shell\"\\)", codon_server_code))
stopifnot(!grepl("if \\(!identical\\(active_group\\(\\), \"Input and Usage\"\\)\\)", codon_server_code))

stopifnot(grepl("if \\(!Array\\.isArray\\(sections\\) \\|\\| sections.length === 0\\) \\{[[:space:]]*return currentState;[[:space:]]*\\}", module_controls_code))
stopifnot(grepl("id = ns\\(\"analysis_summary_shell\"\\)", codon_ui_code))
stopifnot(grepl("class = \"ribote-codon-analysis-summary-shell\"", codon_ui_code, fixed = TRUE))

cat("ribote codon controls state guard test passed\n")
