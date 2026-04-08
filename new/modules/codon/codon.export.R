codon_table_to_delimited_text <- function(data_frame, format = "csv") {
  table_frame <- if (is.data.frame(data_frame)) data_frame else data.frame()

  if (!nrow(table_frame)) {
    return(paste(colnames(table_frame), collapse = if (identical(format, "txt")) "\t" else ","))  
  }

  con <- textConnection("output", "w", local = TRUE)
  on.exit(close(con), add = TRUE)
  utils::write.table(
    table_frame,
    file = con,
    sep = if (identical(format, "txt")) "\t" else ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    na = ""
  )
  paste(output, collapse = "\n")
}

codon_export_tables <- function(context) {
  stopifnot(is.list(context))

  empty_input_summary <- data.frame(
    GeneID = character(),
    Gene_Name = character(),
    TE_Group = character(),
    RNA_Control_Mean = numeric(),
    RNA_Treatment_Mean = numeric(),
    Total_Codons = numeric(),
    Selected_Codon_Count = numeric(),
    Selected_Codon_per_1k = numeric(),
    Selected_Codon_Frequency = numeric(),
    stringsAsFactors = FALSE
  )

  list(
    input_summary = if (!is.null(context$scope_context)) codon_input_summary_table(context$scope_context) else empty_input_summary,
    selected_codon_usage = {
      rows <- context$results$selectedCodonUsage$rows
      if (length(rows)) {
        as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE)
      } else {
        data.frame(
          Codon = character(),
          Genes_Measured = numeric(),
          Up_Median_Percent = numeric(),
          Non_Median_Percent = numeric(),
          Down_Median_Percent = numeric(),
          Up_vs_Non_PValue = numeric(),
          Down_vs_Non_PValue = numeric(),
          stringsAsFactors = FALSE
        )
      }
    },
    selected_codon_vs_rna = {
      rows <- context$results$selectedCodonVsRna$rows
      if (length(rows)) {
        as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE)
      } else {
        data.frame(
          Codon = character(),
          RNA_Group = character(),
          Genes_Measured = numeric(),
          Displayed_Genes = numeric(),
          Pearson_R = numeric(),
          P_Value = numeric(),
          stringsAsFactors = FALSE
        )
      }
    },
    cbi_tai_by_group = {
      rows <- context$results$cbiTaiByGroup$rows
      if (length(rows)) {
        as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE)
      } else {
        data.frame(
          Metric = character(),
          Genes_Measured = numeric(),
          Up_Median = numeric(),
          Non_Median = numeric(),
          Down_Median = numeric(),
          Up_vs_Non_PValue = numeric(),
          Down_vs_Non_PValue = numeric(),
          stringsAsFactors = FALSE
        )
      }
    },
    cbi_associations = {
      rows <- context$results$cbiAssociations$rows
      if (length(rows)) {
        as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE)
      } else {
        data.frame(
          Association = character(),
          Condition = character(),
          Genes_Measured = numeric(),
          Displayed_Genes = numeric(),
          Pearson_R = numeric(),
          P_Value = numeric(),
          stringsAsFactors = FALSE
        )
      }
    },
    selected_codon_burden = {
      rows <- context$results$selectedCodonBurden$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Scope = character(), TE_Group = character(), Genes_Measured = numeric(), Displayed_Genes = numeric(), Pearson_R = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
    },
    codon_enrichment_shifted = {
      rows <- context$results$codonEnrichmentShifted$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(codon = character(), Up_Genes = numeric(), Non_Genes = numeric(), Down_Genes = numeric(), Shifted_Genes = numeric(), Genes_With_Hits = numeric(), Log2_Up_vs_Down = numeric(), Shifted_Fraction = numeric(), Selected = logical(), stringsAsFactors = FALSE)
    },
    selected_codon_across_groups = {
      rows <- context$results$selectedCodonAcrossGroups$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Metric = character(), Genes_Measured = numeric(), Up_Median = numeric(), Non_Median = numeric(), Down_Median = numeric(), Up_vs_Non_PValue = numeric(), Down_vs_Non_PValue = numeric(), stringsAsFactors = FALSE)
    },
    permutation_support = {
      rows <- context$results$permutationSupport$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Metric = character(), Observed = numeric(), Permutation_Mean = numeric(), Empirical_PValue = numeric(), Iterations = numeric(), stringsAsFactors = FALSE)
    },
    te_bias_selected_load = {
      rows <- context$results$teBiasSelectedLoad$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Load_Bin = character(), Genes = numeric(), Median_Selected_Load = numeric(), Mean_TE_log2FC = numeric(), Up_Fraction = numeric(), Non_Fraction = numeric(), Down_Fraction = numeric(), stringsAsFactors = FALSE)
    },
    selected_load_effect = {
      rows <- context$results$selectedLoadEffect$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Scope = character(), Genes_Measured = numeric(), Displayed_Genes = numeric(), Pearson_R = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
    },
    codon_clustering = {
      rows <- context$results$codonClustering$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Codon = character(), Mean_Correlation = numeric(), Selected = logical(), stringsAsFactors = FALSE)
    },
    codon_usage_heatmap = {
      rows <- context$results$codonUsageHeatmap$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Displayed_Genes = numeric(), Available_Genes = numeric(), Codons = numeric(), stringsAsFactors = FALSE)
    },
    codon_run_zscore = {
      rows <- context$results$codonRunZscore$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Run_Length = character(), Genes_Measured = numeric(), Displayed_Genes = numeric(), Pearson_R = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
    },
    codon_run_enrichment = {
      rows <- context$results$codonRunEnrichment$rows
      if (length(rows)) as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE) else data.frame(Metric = character(), Genes_Measured = numeric(), Up_Median = numeric(), Non_Median = numeric(), Down_Median = numeric(), Up_vs_Non_PValue = numeric(), Down_vs_Non_PValue = numeric(), stringsAsFactors = FALSE)
    }
  )
 
}

codon_data_export_entries <- function(context, format = "csv", view = "input_summary") {
  extension <- if (identical(format, "txt")) "txt" else "csv"
  tables <- codon_export_tables(context)
  active_view <- codon_normalize_result_view(view)

  entry_catalog <- list(
    input_summary = list(
      filename = codon_data_filename("input_summary", extension = extension),
      content = codon_table_to_delimited_text(tables$input_summary, format = format)
    ),
    selected_codon_usage = list(
      filename = codon_data_filename("selected_codon_usage_summary", extension = extension),
      content = codon_table_to_delimited_text(tables$selected_codon_usage, format = format)
    ),
    selected_codon_vs_rna = list(
      filename = codon_data_filename("selected_codon_vs_rna_summary", extension = extension),
      content = codon_table_to_delimited_text(tables$selected_codon_vs_rna, format = format)
    ),
    cbi_tai_by_group = list(
      filename = codon_data_filename("codon_bias_and_adaptation_by_te_group", extension = extension),
      content = codon_table_to_delimited_text(tables$cbi_tai_by_group, format = format)
    ),
    cbi_associations = list(
      filename = codon_data_filename("cbi_associations", extension = extension),
      content = codon_table_to_delimited_text(tables$cbi_associations, format = format)
    ),
    selected_codon_burden = list(
      filename = codon_data_filename("selected_codon_burden", extension = extension),
      content = codon_table_to_delimited_text(tables$selected_codon_burden, format = format)
    ),
    codon_enrichment_shifted = list(
      filename = codon_data_filename("codon_enrichment_shifted", extension = extension),
      content = codon_table_to_delimited_text(tables$codon_enrichment_shifted, format = format)
    ),
    selected_codon_across_groups = list(
      filename = codon_data_filename("selected_codon_across_groups", extension = extension),
      content = codon_table_to_delimited_text(tables$selected_codon_across_groups, format = format)
    ),
    permutation_support = list(
      filename = codon_data_filename("permutation_support", extension = extension),
      content = codon_table_to_delimited_text(tables$permutation_support, format = format)
    ),
    te_bias_selected_load = list(
      filename = codon_data_filename("te_bias_selected_load", extension = extension),
      content = codon_table_to_delimited_text(tables$te_bias_selected_load, format = format)
    ),
    selected_load_effect = list(
      filename = codon_data_filename("selected_load_effect", extension = extension),
      content = codon_table_to_delimited_text(tables$selected_load_effect, format = format)
    ),
    codon_clustering = list(
      filename = codon_data_filename("codon_clustering", extension = extension),
      content = codon_table_to_delimited_text(tables$codon_clustering, format = format)
    ),
    codon_usage_heatmap = list(
      filename = codon_data_filename("codon_usage_heatmap", extension = extension),
      content = codon_table_to_delimited_text(tables$codon_usage_heatmap, format = format)
    ),
    codon_run_zscore = list(
      filename = codon_data_filename("codon_run_zscore", extension = extension),
      content = codon_table_to_delimited_text(tables$codon_run_zscore, format = format)
    ),
    codon_run_enrichment = list(
      filename = codon_data_filename("codon_run_enrichment", extension = extension),
      content = codon_table_to_delimited_text(tables$codon_run_enrichment, format = format)
    )
  )

  list(entry_catalog[[active_view]])
}

