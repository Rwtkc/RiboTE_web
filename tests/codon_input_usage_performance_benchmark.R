source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/shared/module_shell.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/pca/pca.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/codon/codon.shared.R", local = TRUE, encoding = "UTF-8")

build_demo_codon_workspace <- function() {
  gene_matrix <- read.table("TEShinyData/all.count.txt", header = TRUE, sep = "\t", check.names = FALSE)
  species_label <- "Homo sapiens (hg38)"
  species_meta <- ribote_species_meta(species_label)

  upload_context <- list(
    species = species_label,
    species_meta = species_meta,
    pair_manifest = data.frame(
      rna_sample = c("RNA.WT1", "RNA.WT2", "RNA.KO1", "RNA.KO2"),
      ribo_sample = c("RPF.WT1", "RPF.WT2", "RPF.KO1", "RPF.KO2"),
      group_role = c("Control", "Control", "Treatment", "Treatment"),
      stringsAsFactors = FALSE
    ),
    resource_paths = list(
      gff_rda_path = app_data_path("gff", species_meta$gff_rda_name),
      txlens_path = app_data_path("txlens", species_meta$txlens_name),
      fasta_path = app_data_path("fa", species_meta$fasta_name),
      tai_path = app_data_path("cds", species_meta$tai_name),
      cbi_path = app_data_path("cds", species_meta$cbi_name)
    )
  )

  preprocess_context <- list(
    preview = gene_matrix,
    parameters = list(min_cpm = 0.5, min_libraries = 1L)
  )

  te_context <- translation_efficiency_compute_context(
    preprocess_context = preprocess_context,
    upload_context = upload_context,
    te_tool = "Riborex",
    fvalue = 1.5,
    p_cutoff = 0.05,
    p_type = "Fdr"
  )

  resource_context <- codon_build_resource_context(upload_context)
  base_context <- codon_build_base_context(
    te_context = te_context,
    preprocess_context = preprocess_context,
    upload_context = upload_context,
    resource_context = resource_context
  )

  list(base_context = base_context)
}

naive_codon_build_scope_table <- function(base_context, parameters) {
  gene_table <- as.data.frame(base_context$gene_table, stringsAsFactors = FALSE)

  selected_summary <- if (length(parameters$codon_select)) {
    selected_rows <- base_context$codon_table[base_context$codon_table$codon %in% parameters$codon_select, , drop = FALSE]
    if (nrow(selected_rows)) {
      aggregate(
        cbind(count, frequency, per1k) ~ GeneKey,
        data = transform(selected_rows, count = as.numeric(count), frequency = as.numeric(frequency), per1k = as.numeric(per1k)),
        FUN = sum,
        na.rm = TRUE
      )
    } else {
      data.frame(GeneKey = character(), count = numeric(), frequency = numeric(), per1k = numeric(), stringsAsFactors = FALSE)
    }
  } else {
    data.frame(GeneKey = character(), count = numeric(), frequency = numeric(), per1k = numeric(), stringsAsFactors = FALSE)
  }

  colnames(selected_summary) <- c("GeneKey", "selectedCodonCount", "selectedCodonFrequency", "selectedCodonPer1k")
  gene_scope <- merge(gene_table, selected_summary, by = "GeneKey", all.x = TRUE)
  gene_scope$selectedCodonCount[is.na(gene_scope$selectedCodonCount)] <- 0
  gene_scope$selectedCodonFrequency[is.na(gene_scope$selectedCodonFrequency)] <- 0
  gene_scope$selectedCodonPer1k[is.na(gene_scope$selectedCodonPer1k)] <- 0

  effective_scope <- if (identical(parameters$codon_display, "Obj") && length(parameters$codon_select)) "Obj" else "All"
  if (identical(effective_scope, "Obj")) {
    gene_scope <- gene_scope[gene_scope$selectedCodonCount > 0, , drop = FALSE]
  }

  list(
    effective_scope = effective_scope,
    label = if (identical(effective_scope, "Obj")) "Selected-codon genes" else "All codon-ready genes",
    note = if (identical(parameters$codon_display, "Obj") && !length(parameters$codon_select)) {
      "No codons are selected yet, so the current gene scope falls back to all codon-ready genes."
    } else {
      NULL
    },
    gene_table = gene_scope
  )
}

naive_codon_build_selected_usage <- function(scope_context, base_context, parameters) {
  if (!length(parameters$codon_select)) {
    return(list(note = "Select one or more codons to compare their CDS usage across Translation Efficiency groups.", scopeLabel = scope_context$label, focus = parameters$codon_direction, panels = list(), rows = list()))
  }

  scope_table <- scope_context$gene_table
  if (!nrow(scope_table)) {
    return(list(note = "No genes are available in the current scope for the selected codons.", scopeLabel = scope_context$label, focus = parameters$codon_direction, panels = list(), rows = list()))
  }

  usage_table <- merge(
    base_context$codon_table[base_context$codon_table$codon %in% parameters$codon_select, , drop = FALSE],
    scope_table[, c("GeneKey", "TE_Status"), drop = FALSE],
    by = c("GeneKey", "TE_Status"),
    all = FALSE
  )

  if (!nrow(usage_table)) {
    return(list(note = "The selected codons were not observed in the current gene scope.", scopeLabel = scope_context$label, focus = parameters$codon_direction, panels = list(), rows = list()))
  }

  usage_split <- split(usage_table, usage_table$codon)
  summary_rows <- do.call(rbind, lapply(parameters$codon_select, function(codon_value) {
    if (!codon_value %in% names(usage_split)) {
      return(NULL)
    }
    codon_usage_summary_row(codon_value, usage_split[[codon_value]])
  }))
  if (is.null(summary_rows)) {
    summary_rows <- data.frame()
  }

  if (nrow(summary_rows)) {
    sort_metric <- if (identical(parameters$codon_direction, "Up")) {
      summary_rows$Up_vs_Non_PValue
    } else if (identical(parameters$codon_direction, "Down")) {
      summary_rows$Down_vs_Non_PValue
    } else {
      pmin(summary_rows$Up_vs_Non_PValue, summary_rows$Down_vs_Non_PValue, na.rm = TRUE)
    }
    sort_metric[!is.finite(sort_metric)] <- Inf
    summary_rows <- summary_rows[order(sort_metric, summary_rows$Codon), , drop = FALSE]
  }

  visualized_codons <- parameters$codon_select
  visual_limit <- codon_max_visualized_codons()
  if (length(visualized_codons) > visual_limit) {
    visualized_codons <- visualized_codons[seq_len(visual_limit)]
  }

  panels <- lapply(visualized_codons, function(codon_value) {
    codon_usage_panel_payload(codon_value, usage_split[[codon_value]])
  })

  list(
    note = paste(
      sprintf("Current gene scope: %s.", scope_context$label),
      "Values show the fraction of codons within each gene CDS that match the selected codon.",
      "Wilcoxon tests compare Up vs Non and Down vs Non within the same current scope."
    ),
    scopeLabel = scope_context$label,
    focus = parameters$codon_direction,
    panels = panels,
    rows = codon_rows_payload(summary_rows)
  )
}

naive_codon_build_selected_vs_rna <- function(scope_context, base_context, parameters) {
  if (!length(parameters$codon_select)) {
    return(list(note = "Select one or more codons to relate codon usage to RNA abundance.", scopeLabel = scope_context$label, panels = list(), rows = list()))
  }

  scope_table <- scope_context$gene_table
  if (!nrow(scope_table)) {
    return(list(note = "No genes are available in the current scope for the selected codons.", scopeLabel = scope_context$label, panels = list(), rows = list()))
  }

  scatter_source <- merge(
    base_context$codon_table[base_context$codon_table$codon %in% parameters$codon_select, , drop = FALSE],
    scope_table[, c("GeneKey", "GeneID", "gene_name", "TE_Status", "RNA_Control_Mean", "RNA_Treatment_Mean"), drop = FALSE],
    by = c("GeneKey", "GeneID", "gene_name", "TE_Status", "RNA_Control_Mean", "RNA_Treatment_Mean"),
    all = FALSE
  )

  if (!nrow(scatter_source)) {
    return(list(note = "The selected codons were not observed in the current gene scope.", scopeLabel = scope_context$label, panels = list(), rows = list()))
  }

  scatter_split <- split(scatter_source, scatter_source$codon)
  summary_rows <- do.call(rbind, lapply(parameters$codon_select, function(codon_value) {
    if (!codon_value %in% names(scatter_split)) {
      return(NULL)
    }

    codon_table <- scatter_split[[codon_value]]
    control_stats <- codon_correlation_stats(as.numeric(codon_table$frequency) * 100, log2(pmax(as.numeric(codon_table$RNA_Control_Mean), 0) + 1))
    treatment_stats <- codon_correlation_stats(as.numeric(codon_table$frequency) * 100, log2(pmax(as.numeric(codon_table$RNA_Treatment_Mean), 0) + 1))

    rbind(
      data.frame(Codon = codon_value, RNA_Group = "Control", Genes_Measured = control_stats$geneCount, Displayed_Genes = min(control_stats$geneCount, codon_scatter_display_limit()), Pearson_R = control_stats$correlation, P_Value = control_stats$pValue, stringsAsFactors = FALSE),
      data.frame(Codon = codon_value, RNA_Group = "Treatment", Genes_Measured = treatment_stats$geneCount, Displayed_Genes = min(treatment_stats$geneCount, codon_scatter_display_limit()), Pearson_R = treatment_stats$correlation, P_Value = treatment_stats$pValue, stringsAsFactors = FALSE)
    )
  }))
  if (is.null(summary_rows)) {
    summary_rows <- data.frame()
  }

  visualized_codons <- parameters$codon_select
  visual_limit <- codon_max_visualized_codons()
  if (length(visualized_codons) > visual_limit) {
    visualized_codons <- visualized_codons[seq_len(visual_limit)]
  }

  panels <- lapply(visualized_codons, function(codon_value) {
    codon_scatter_panel_payload(codon_value, scatter_split[[codon_value]])
  })

  list(
    note = paste(
      sprintf("Current gene scope: %s.", scope_context$label),
      "Each point is one gene, and RNA abundance is shown as log2(mean normalized abundance + 1) to keep wide abundance ranges readable.",
      sprintf(
        "When more than %s genes are available for a codon-condition pair, the plotted points are a deterministic display subset, but Pearson correlations still use the full gene set.",
        format(codon_scatter_display_limit(), big.mark = ",")
      )
    ),
    scopeLabel = scope_context$label,
    panels = panels,
    rows = codon_rows_payload(summary_rows)
  )
}

naive_codon_compute_context <- function(base_context, parameters) {
  scope_context <- naive_codon_build_scope_table(base_context, parameters)
  selected_count <- length(parameters$codon_select)
  genes_ready <- nrow(base_context$gene_table)
  genes_in_scope <- nrow(scope_context$gene_table)
  genes_with_selected_codons <- if (selected_count) sum(scope_context$gene_table$selectedCodonCount > 0, na.rm = TRUE) else 0L

  list(
    source_signature = base_context$source_signature,
    parameters = parameters,
    scope_context = scope_context,
    metrics = list(
      list(label = "Codon-Ready Genes", value = format(genes_ready, big.mark = ",")),
      list(label = "Current Scope", value = format(genes_in_scope, big.mark = ",")),
      list(label = "Selected Codons", value = format(selected_count, big.mark = ",")),
      list(label = "Genes With Selected Codons", value = format(genes_with_selected_codons, big.mark = ","))
    ),
    results = list(
      note = scope_context$note,
      selectedCodons = as.list(parameters$codon_select),
      geneScopeLabel = scope_context$label,
      teGroupFocus = parameters$codon_direction,
      views = codon_views_payload(),
      inputSummary = codon_build_input_summary(scope_context, parameters),
      selectedCodonUsage = naive_codon_build_selected_usage(scope_context, base_context, parameters),
      selectedCodonVsRna = naive_codon_build_selected_vs_rna(scope_context, base_context, parameters),
      cbiTaiByGroup = list(),
      cbiAssociations = list()
    )
  )
}

time_min <- function(expr, iterations = 3L) {
  quoted_expr <- substitute(expr)
  parent_env <- parent.frame()
  elapsed <- numeric(iterations)
  value <- NULL
  for (index in seq_len(iterations)) {
    gc()
    started <- proc.time()[["elapsed"]]
    value <- eval(quoted_expr, envir = parent_env)
    elapsed[[index]] <- proc.time()[["elapsed"]] - started
  }
  list(seconds = min(elapsed), value = value)
}

assert_equivalent <- function(reference, candidate, label) {
  comparison <- all.equal(reference, candidate, tolerance = 1e-10, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(sprintf("%s mismatch: %s", label, paste(comparison, collapse = "; ")), call. = FALSE)
  }
}

benchmark_case <- function(base_context, label, codon_select, codon_display) {
  parameters <- codon_normalize_parameters(
    codon_select = codon_select,
    codon_direction = "Up",
    codon_display = codon_display
  )

  reference_run <- time_min(naive_codon_compute_context(base_context, parameters), iterations = 3L)
  optimized_run <- time_min(codon_compute_context(base_context, parameters), iterations = 3L)
  assert_equivalent(reference_run$value, optimized_run$value, label)

  cat(sprintf(
    "%s naive_seconds=%.3f optimized_seconds=%.3f speedup=%.2fx selected_codons=%d scope=%s\n",
    label,
    reference_run$seconds,
    optimized_run$seconds,
    reference_run$seconds / optimized_run$seconds,
    length(parameters$codon_select),
    parameters$codon_display
  ))
}

workspace <- build_demo_codon_workspace()

benchmark_case(
  base_context = workspace$base_context,
  label = "single_selected_obj",
  codon_select = "AAA",
  codon_display = "Obj"
)

benchmark_case(
  base_context = workspace$base_context,
  label = "visual_limit_selected_obj",
  codon_select = utils::head(codon_sense_values(), codon_max_visualized_codons()),
  codon_display = "Obj"
)

cat("codon input usage performance benchmark passed\n")
