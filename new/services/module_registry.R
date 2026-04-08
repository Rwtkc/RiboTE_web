ribote_module_registry <- function() {
  list(
    list(id = "load_data", title = "Load Data", hint = "Species and matrix context"),
    list(id = "data_preprocess", title = "Data Preprocess", hint = "Filtering and QC placeholders"),
    list(id = "translation_efficiency", title = "Translation Efficiency", hint = "TE workspace shell"),
    list(id = "pca", title = "PCA", hint = "Sample-space projection"),
    list(id = "clustering", title = "Clustering", hint = "Heatmap and sub-heatmap"),
    list(id = "gsea", title = "GSEA", hint = "Pathway ranking views"),
    list(id = "enrichment", title = "Enrichment", hint = "Term-level summaries"),
    list(id = "network", title = "Network", hint = "Module and graph canvas"),
    list(id = "signalp", title = "SignalP", hint = "Signal peptide placeholder"),
    list(id = "codon", title = "Codon", hint = "Codon-centric analysis stack")
  )
}
