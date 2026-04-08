source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/gsea/gsea.shared.R", local = TRUE, encoding = "UTF-8")

human_collections <- gsea_collection_values("hg38")
rice_collections <- gsea_collection_values("osa_IRGSP_1")

stopifnot(identical(human_collections, c("hallmark", "reactome", "go_bp")))
stopifnot(identical(rice_collections, c("go_bp", "go_mf", "go_cc", "kegg")))
stopifnot(!any(c("hallmark", "reactome") %in% rice_collections))
stopifnot(!any(c("go_mf", "go_cc", "kegg") %in% human_collections))
stopifnot(identical(gsea_default_collection("osa_IRGSP_1"), "go_bp"))

module_controls_code <- paste(
  readLines("new/frontend/app_shell/src/components/ModuleControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
gsea_server_code <- paste(
  readLines("new/modules/gsea/gsea.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("function buildPreservedState", module_controls_code, fixed = TRUE))
stopifnot(grepl("function shallowStateEqual", module_controls_code, fixed = TRUE))
stopifnot(grepl("function buildSpeciesScopedSections", module_controls_code, fixed = TRUE))
stopifnot(grepl("scopeId !== \"gsea-controls_host\"", module_controls_code, fixed = TRUE))
stopifnot(grepl("Oryza sativa (IRGSP 1.0)", module_controls_code, fixed = TRUE))
stopifnot(grepl("GO Molecular Function", module_controls_code, fixed = TRUE))
stopifnot(grepl("const effectiveState = useMemo", module_controls_code, fixed = TRUE))
stopifnot(grepl("setShinyInputValue(field.id, effectiveState[field.key]", module_controls_code, fixed = TRUE))
stopifnot(grepl("renderField(field, effectiveState, setState, scopeId)", module_controls_code, fixed = TRUE))
stopifnot(grepl("gsea_collection_options(species_key_value)", gsea_server_code, fixed = TRUE))
stopifnot(grepl("gsea_default_collection(species_key_value)", gsea_server_code, fixed = TRUE))
stopifnot(grepl("last_controls_species_key <- reactiveVal(NULL)", gsea_server_code, fixed = TRUE))
stopifnot(grepl("schedule_publish_controls <- function()", gsea_server_code, fixed = TRUE))
stopifnot(grepl("next_species_key <- species_key()", gsea_server_code, fixed = TRUE))
stopifnot(grepl("schedule_publish_controls()", gsea_server_code, fixed = TRUE))

cat("ribote gsea species collection controls test passed\n")
