codon_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/CodonResults.jsx", warn = FALSE, encoding = "UTF-8"),
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

analysis_bridge_code <- paste(
  readLines("new/frontend/app_shell/src/bridge/analysisActionBridge.js", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

css_code <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("sidebarGroupHostId = session\\$ns\\(\"sidebar_group_host\"\\)", codon_server_code))
stopifnot(grepl("id = ns\\(\"sidebar_group_host\"\\)", codon_ui_code))
stopifnot(grepl("data-ribote-codon-active-group", codon_ui_code, fixed = TRUE))
stopifnot(grepl("host.dataset.riboteCodonActiveGroup = activeGroup", codon_results_code, fixed = TRUE))
stopifnot(!grepl("pendingServerActiveViewRef", codon_results_code, fixed = TRUE))
stopifnot(grepl("const dispatchActiveViewRequest = \\(request\\) => \\{[[:space:]]*if \\(!request \\|\\| !activeViewInputId \\|\\| analysisLocked\\) \\{[[:space:]]*return false;[[:space:]]*\\}", codon_results_code))
stopifnot(grepl("if \\(!activeViewInputId \\|\\| analysisLocked\\) \\{[[:space:]]*return undefined;[[:space:]]*\\}", codon_results_code))
stopifnot(grepl("window\\.dispatchEvent\\(new CustomEvent\\(\"rnameta:analysis-lock-state\"", analysis_bridge_code))
stopifnot(grepl("uiOutput\\(ns\\(\"run_input_usage_hint\"\\)\\)", codon_ui_code))
stopifnot(grepl("uiOutput\\(ns\\(\"run_codon_bias_hint\"\\)\\)", codon_ui_code))
stopifnot(grepl("react_control_host\\([[:space:]]*id = ns\\(\"controls_host\"\\)", codon_ui_code))
stopifnot(grepl("\\.ribote-codon-sidebar-panel \\{", css_code))
stopifnot(grepl("`data-ribote-codon-group` = \"Codon Bias\"", codon_ui_code, fixed = TRUE))
stopifnot(grepl("\\.ribote-codon-sidebar-shell\\[data-ribote-codon-active-group=\"Codon Bias\"\\]", css_code))

cat("ribote codon sidebar lock sync test passed\n")
