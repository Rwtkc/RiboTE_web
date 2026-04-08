te_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/TranslationEfficiencyResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

preprocess_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/PreprocessResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("readPersistedActiveView\\(persistedActiveViewKey\\) \\|\\| configuredActiveView", te_results_code))
stopifnot(grepl("useRef\\(null\\)", te_results_code))
stopifnot(grepl("pendingActiveViewRef", te_results_code, fixed = TRUE))
stopifnot(grepl("pendingActiveViewRef.current = nextTab", te_results_code, fixed = TRUE))
stopifnot(grepl("if \\(pendingView\\)", te_results_code))
stopifnot(grepl("if \\(configuredActiveView === pendingView\\)", te_results_code))
stopifnot(grepl("pendingActiveViewRef.current = null", te_results_code, fixed = TRUE))
stopifnot(grepl("window.__riboteActiveViewState", te_results_code, fixed = TRUE))
stopifnot(grepl("persistedActiveViewKey", te_results_code, fixed = TRUE))
stopifnot(grepl("if \\(!configuredChanged && persistedActiveView && activeTab === persistedActiveView\\)", te_results_code))
stopifnot(grepl("onClick=\\{\\(\\) => handleTabChange\\(tab.id\\)\\}", te_results_code))
stopifnot(grepl("if \\(!activeViewInputId \\|\\| activeTab === configuredActiveView\\)", te_results_code))
stopifnot(grepl("readPersistedActiveView\\(persistedActiveViewKey\\) \\|\\| configuredActiveView", preprocess_results_code))
stopifnot(grepl("useRef\\(null\\)", preprocess_results_code))
stopifnot(grepl("pendingActiveViewRef", preprocess_results_code, fixed = TRUE))
stopifnot(grepl("pendingActiveViewRef.current = nextTab", preprocess_results_code, fixed = TRUE))
stopifnot(grepl("if \\(pendingView\\)", preprocess_results_code))
stopifnot(grepl("if \\(configuredActiveView === pendingView\\)", preprocess_results_code))
stopifnot(grepl("pendingActiveViewRef.current = null", preprocess_results_code, fixed = TRUE))
stopifnot(grepl("window.__riboteActiveViewState", preprocess_results_code, fixed = TRUE))
stopifnot(grepl("persistedActiveViewKey", preprocess_results_code, fixed = TRUE))
stopifnot(grepl("if \\(!configuredChanged && persistedActiveView && activeTab === persistedActiveView\\)", preprocess_results_code))
stopifnot(grepl("onClick=\\{\\(\\) => handleTabChange\\(tab.id\\)\\}", preprocess_results_code))
stopifnot(grepl("if \\(!activeViewInputId \\|\\| activeTab === configuredActiveView\\)", preprocess_results_code))

cat("ribote results active tab sync test passed\n")
