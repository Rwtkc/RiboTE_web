codon_server_code <- paste(
  readLines("new/modules/codon/codon.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_results_code <- paste(
  c(
    readLines("new/frontend/app_shell/src/components/CodonResults.jsx", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/CodonResultsViews.jsx", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/CodonResultsViewUtils.jsx", warn = FALSE, encoding = "UTF-8")
  ),
  collapse = "\n"
)

codon_chart_code <- paste(
  c(
    readLines("new/frontend/app_shell/src/components/codonChart.js", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/codonChart.shared.js", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/codonChart.group.js", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/codonChart.scatter.js", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/codonChart.enrichment.js", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/codonChart.load.js", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/codonChart.heatmap.js", warn = FALSE, encoding = "UTF-8")
  ),
  collapse = "\n"
)

stopifnot(grepl(
  "ready = !is.null\\(context\\) && current_view\\(\\) %in% codon_supported_result_views\\(\\) && codon_view_has_data\\(context, current_view\\(\\)\\)",
  codon_server_code
))
stopifnot(grepl("activeViewRequestSeq = latest_result_view_request_seq\\(\\)", codon_server_code))
stopifnot(grepl("\\} else if \\(seq_advanced\\) \\{[[:space:]]*publish_results\\(\\)[[:space:]]*publish_export\\(\\)[[:space:]]*\\}", codon_server_code))

stopifnot(grepl("const hasValidLocalView = activeView && validViewIds.has\\(activeView\\);", codon_results_code))
stopifnot(grepl("const configuredTokenChanged = lastConfiguredActiveViewTokenRef.current !== configuredActiveViewToken;", codon_results_code))
stopifnot(grepl("const configuredRequestSeqChanged = lastConfiguredActiveViewRequestSeqRef.current !== configuredActiveViewRequestSeq;", codon_results_code))
stopifnot(grepl("const requestAcknowledged = configuredActiveView === pendingView.id[[:space:]]*&& configuredActiveViewRequestSeq >= pendingView.requestSeq;", codon_results_code))
stopifnot(grepl("if \\(requestAcknowledged\\) \\{[[:space:]]*pendingActiveViewRef.current = null;", codon_results_code))
stopifnot(grepl("if \\(queuedView\\.id === configuredActiveView && configuredActiveViewRequestSeq >= queuedView\\.requestSeq\\) \\{[[:space:]]*queuedActiveViewRef.current = null;", codon_results_code))
stopifnot(grepl("\\} else if \\(dispatchActiveViewRequest\\(queuedView\\)\\) \\{[[:space:]]*queuedActiveViewRef.current = null;[[:space:]]*(if \\(persistedActiveViewKey\\) \\{[[:space:]]*ensureCodonViewRequestStore\\(\\)\\[persistedActiveViewKey\\] = \\{[[:space:]]*pending: pendingActiveViewRef.current,[[:space:]]*queued: null,[[:space:]]*nextRequestSeq: nextResultViewRequestSeqRef.current[[:space:]]*\\};[[:space:]]*\\})?[[:space:]]*return;", codon_results_code))
stopifnot(grepl("nextResultViewRequestSeqRef", codon_results_code, fixed = TRUE))
stopifnot(grepl("queuedActiveViewRef", codon_results_code, fixed = TRUE))
stopifnot(grepl("seq: request.requestSeq", codon_results_code, fixed = TRUE))
stopifnot(grepl("if \\(!pendingActiveViewRef\\.current && queuedActiveViewRef\\.current\\) \\{[[:space:]]*if \\(dispatchActiveViewRequest\\(queuedActiveViewRef\\.current\\)\\) \\{[[:space:]]*queuedActiveViewRef.current = null;", codon_results_code))
stopifnot(grepl("if \\(!dispatchActiveViewRequest\\(nextRequest\\)\\) \\{[[:space:]]*queuedActiveViewRef.current = nextRequest;", codon_results_code))
stopifnot(grepl("requestSeq: nextResultViewRequestSeqRef.current \\+ 1", codon_results_code))

stopifnot(grepl("function buildLinearDomain\\(", codon_chart_code))
stopifnot(grepl("\\.clamp\\(true\\)", codon_chart_code))
stopifnot(grepl("function appendPlotClip\\(", codon_chart_code))
stopifnot(grepl("const clipId = nextClipId\\(\"ribote-codon-bias-group-clip\"\\);", codon_chart_code))
stopifnot(grepl("const clipId = nextClipId\\(\"ribote-codon-cbi-association-clip\"\\);", codon_chart_code))

cat("ribote codon regression guards test passed\n")
