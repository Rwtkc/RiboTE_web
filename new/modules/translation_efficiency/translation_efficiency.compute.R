translation_efficiency_deseq2_rex <- function(
  rna_count_table,
  ribo_count_table,
  rna_cond,
  ribo_cond,
  min_counts = 0.5,
  n_min_samples = 1L,
  min_mean_count = 1
) {
  if (!requireNamespace("DESeq2", quietly = TRUE) ||
      !requireNamespace("edgeR", quietly = TRUE) ||
      !requireNamespace("data.table", quietly = TRUE)) {
    stop("DESeq2, edgeR, and data.table are required for Riborex TE analysis.", call. = FALSE)
  }

  if (!identical(rownames(rna_count_table), rownames(ribo_count_table))) {
    stop("RNA-seq and Ribo-seq data must have the same set of genes.", call. = FALSE)
  }

  if (!is.data.frame(rna_cond)) {
    rna_cond <- data.frame(cond = rna_cond, stringsAsFactors = FALSE)
  }

  if (!is.data.frame(ribo_cond)) {
    ribo_cond <- data.frame(cond = ribo_cond, stringsAsFactors = FALSE)
  }

  keep_rna <- rownames(rna_count_table)[rowMeans(rna_count_table, na.rm = TRUE) >= min_mean_count]
  keep_ribo <- rownames(ribo_count_table)[rowMeans(ribo_count_table, na.rm = TRUE) >= min_mean_count]
  keep <- intersect(keep_rna, keep_ribo)

  rna_count_table <- rna_count_table[keep, , drop = FALSE]
  ribo_count_table <- ribo_count_table[keep, , drop = FALSE]

  num_cond <- ncol(rna_cond)
  num_rna_samples <- nrow(rna_cond)
  num_ribo_samples <- nrow(ribo_cond)
  combined_count_table <- cbind(rna_count_table, ribo_count_table)
  combined_cond <- rbind(rna_cond, ribo_cond)
  combined_cond <- combined_cond[, rep(seq_len(ncol(combined_cond)), 2), drop = FALSE]
  intercept <- c(rep("CONTROL", num_rna_samples), rep("TREATED", num_ribo_samples))
  combined_cond <- cbind(
    combined_cond[, seq_len(num_cond), drop = FALSE],
    INTERCEPT = intercept,
    combined_cond[, (num_cond + 1):ncol(combined_cond), drop = FALSE]
  )

  for (index in seq.int(num_cond + 2L, ncol(combined_cond))) {
    combined_cond[seq_len(num_rna_samples), index] <- combined_cond[[index]][[1]]
  }

  colnames(combined_cond)[(num_cond + 2L):ncol(combined_cond)] <- paste0("EXTRA", seq_len(num_cond))
  formula_terms <- colnames(combined_cond)
  design_formula <- stats::as.formula(paste("~", paste(formula_terms, collapse = "+")))

  combined_count_table <- round(combined_count_table, 0)
  keep_rows <- apply(
    edgeR::cpm(edgeR::DGEList(counts = combined_count_table)),
    1,
    function(values) sum(values >= min_counts, na.rm = TRUE)
  ) >= as.integer(n_min_samples)
  combined_count_table <- combined_count_table[keep_rows, , drop = FALSE]

  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = combined_count_table,
    colData = combined_cond,
    design = design_formula
  )
  dds <- DESeq2::DESeq(dds, quiet = TRUE)

  normalized_counts <- as.data.frame(DESeq2::counts(dds, normalized = TRUE))
  normalized_counts <- data.table::setDT(normalized_counts, keep.rownames = "GeneID")
  results_dt <- as.data.frame(DESeq2::results(dds))
  results_dt <- data.table::setDT(results_dt, keep.rownames = "GeneID")
  results_dt[, c("baseMean", "lfcSE", "stat") := NULL]
  results_dt[is.na(padj), padj := 1]
  merged <- merge(normalized_counts, results_dt, by = "GeneID", all.y = TRUE)
  merged[order(padj)]
}

translation_efficiency_xtail_normalcount <- function(
  input_matrix,
  rpf_matrix,
  condition,
  min_counts = 0.5,
  n_min_samples = 1L
) {
  if (!requireNamespace("xtail", quietly = TRUE) ||
      !requireNamespace("DESeq2", quietly = TRUE) ||
      !requireNamespace("edgeR", quietly = TRUE) ||
      !requireNamespace("data.table", quietly = TRUE)) {
    stop("xtail, DESeq2, edgeR, and data.table are required for Xtail TE analysis.", call. = FALSE)
  }

  count_data <- cbind(input_matrix, rpf_matrix)
  keep_rows <- apply(
    edgeR::cpm(edgeR::DGEList(counts = count_data)),
    1,
    function(values) sum(values >= min_counts, na.rm = TRUE)
  ) >= as.integer(n_min_samples)
  count_data <- count_data[keep_rows, , drop = FALSE]
  size_factors <- DESeq2::estimateSizeFactorsForMatrix(count_data)
  normalized_counts <- count_data / do.call(rbind, rep(list(size_factors), nrow(count_data)))
  normalized_counts <- data.table::setDT(as.data.frame(normalized_counts), keep.rownames = "GeneID")

  input_matrix <- input_matrix[apply(input_matrix, 1, function(row) all(row != 0)), , drop = FALSE]
  rpf_matrix <- rpf_matrix[apply(rpf_matrix, 1, function(row) all(row != 0)), , drop = FALSE]
  common_genes <- intersect(rownames(input_matrix), rownames(rpf_matrix))
  input_matrix <- input_matrix[common_genes, , drop = FALSE]
  rpf_matrix <- rpf_matrix[common_genes, , drop = FALSE]

  results <- xtail::xtail(
    input_matrix,
    rpf_matrix,
    condition,
    threads = 5,
    bins = 1000
  )
  results_dt <- xtail::resultsTable(results)
  results_dt <- data.table::setDT(as.data.frame(results_dt), keep.rownames = "GeneID")
  results_dt <- results_dt[, .(GeneID, log2FoldChange = log2FC_TE_final, pvalue = pvalue_final, padj = pvalue.adjust)]
  merge(normalized_counts, results_dt, by = "GeneID", all.y = TRUE)
}

translation_efficiency_legacy_normalcount <- function(
  gene_matrix,
  pair_manifest,
  resource_paths = list(),
  te_tool = "Riborex",
  min_counts = 0.5,
  n_min_samples = 1L
) {
  gene_matrix <- translation_efficiency_normalize_input_matrix(gene_matrix)
  pair_manifest <- as.data.frame(pair_manifest, stringsAsFactors = FALSE)
  pair_manifest$rna_sample <- as.character(pair_manifest$rna_sample)
  pair_manifest$ribo_sample <- as.character(pair_manifest$ribo_sample)
  pair_manifest$group_role <- as.character(pair_manifest$group_role)

  control_pairs <- pair_manifest[pair_manifest$group_role == "Control", , drop = FALSE]
  treatment_pairs <- pair_manifest[pair_manifest$group_role == "Treatment", , drop = FALSE]

  inputnames <- c(control_pairs$rna_sample, treatment_pairs$rna_sample)
  rpfnames <- c(control_pairs$ribo_sample, treatment_pairs$ribo_sample)
  condition <- c(rep("G1", nrow(control_pairs)), rep("G2", nrow(treatment_pairs)))

  input_matrix <- gene_matrix[, c("GeneID", inputnames), drop = FALSE]
  input_matrix <- as.data.frame(input_matrix, check.names = FALSE)
  rownames(input_matrix) <- input_matrix$GeneID
  input_matrix <- input_matrix[, -1, drop = FALSE]
  input_matrix[] <- lapply(input_matrix, function(column) as.numeric(as.character(column)))

  rpf_matrix <- gene_matrix[, c("GeneID", rpfnames), drop = FALSE]
  rpf_matrix <- as.data.frame(rpf_matrix, check.names = FALSE)
  rownames(rpf_matrix) <- rpf_matrix$GeneID
  rpf_matrix <- rpf_matrix[, -1, drop = FALSE]
  rpf_matrix[] <- lapply(rpf_matrix, function(column) as.numeric(as.character(column)))

  tool_key <- tolower(as.character(te_tool))
  normalcount <- if (identical(tool_key, "riborex")) {
    translation_efficiency_deseq2_rex(
      rna_count_table = input_matrix,
      ribo_count_table = rpf_matrix,
      rna_cond = condition,
      ribo_cond = condition,
      min_counts = min_counts,
      n_min_samples = n_min_samples
    )
  } else if (identical(tool_key, "xtail")) {
    translation_efficiency_xtail_normalcount(
      input_matrix = input_matrix,
      rpf_matrix = rpf_matrix,
      condition = condition,
      min_counts = min_counts,
      n_min_samples = n_min_samples
    )
  } else {
    stop(sprintf("Unsupported Translation Efficiency tool: %s", te_tool), call. = FALSE)
  }

  normalcount <- as.data.frame(normalcount, check.names = FALSE, stringsAsFactors = FALSE)

  annotation <- translation_efficiency_load_gene_annotation(resource_paths$gff_rda_path)
  if (!is.null(annotation)) {
    normalcount <- merge(annotation, normalcount, by = "GeneID", all.y = TRUE)
    normalcount$gene_name[is.na(normalcount$gene_name) | !nzchar(normalcount$gene_name)] <- "unknown"
  } else {
    normalcount$gene_name <- "unknown"
    normalcount <- normalcount[, c("GeneID", "gene_name", setdiff(colnames(normalcount), c("GeneID", "gene_name"))), drop = FALSE]
  }

  normalcount
}

translation_efficiency_build_base_context <- function(
  preprocess_context,
  upload_context,
  te_tool = "Riborex"
) {
  normalized_parameters <- translation_efficiency_normalize_parameters(te_tool = te_tool)
  gene_matrix <- translation_efficiency_normalize_input_matrix(preprocess_context$preview)
  pair_manifest <- as.data.frame(upload_context$pair_manifest, stringsAsFactors = FALSE)

  stopifnot(is.data.frame(gene_matrix))
  stopifnot(is.data.frame(pair_manifest))
  stopifnot(all(c("rna_sample", "ribo_sample", "group_role") %in% colnames(pair_manifest)))

  control_rna <- pair_manifest$rna_sample[pair_manifest$group_role == "Control"]
  treatment_rna <- pair_manifest$rna_sample[pair_manifest$group_role == "Treatment"]
  control_ribo <- pair_manifest$ribo_sample[pair_manifest$group_role == "Control"]
  treatment_ribo <- pair_manifest$ribo_sample[pair_manifest$group_role == "Treatment"]

  if (length(control_rna) == 0L || length(treatment_rna) == 0L || length(control_ribo) == 0L || length(treatment_ribo) == 0L) {
    stop("Control and Treatment RNA/Ribo pairs are required before running Translation Efficiency.", call. = FALSE)
  }

  if (!all(c(control_rna, treatment_rna, control_ribo, treatment_ribo) %in% colnames(gene_matrix))) {
    stop("Sample pairing does not match the processed matrix columns.", call. = FALSE)
  }

  min_counts <- suppressWarnings(as.numeric(preprocess_context$parameters$min_cpm))
  n_min_samples <- suppressWarnings(as.integer(preprocess_context$parameters$min_libraries))

  if (!is.finite(min_counts) || min_counts < 0) {
    min_counts <- 0.5
  }

  if (!is.finite(n_min_samples) || n_min_samples < 1L) {
    n_min_samples <- 1L
  }

  normalcount <- translation_efficiency_legacy_normalcount(
    gene_matrix = gene_matrix,
    pair_manifest = pair_manifest,
    resource_paths = upload_context$resource_paths,
    te_tool = normalized_parameters$te_tool,
    min_counts = min_counts,
    n_min_samples = n_min_samples
  )

  normalcount$input1 <- rowMeans(normalcount[, control_rna, drop = FALSE], na.rm = TRUE)
  normalcount$input2 <- rowMeans(normalcount[, treatment_rna, drop = FALSE], na.rm = TRUE)
  normalcount$logInputFC <- log2(normalcount$input2 / normalcount$input1)

  normalcount$rpf1 <- rowMeans(normalcount[, control_ribo, drop = FALSE], na.rm = TRUE)
  normalcount$rpf2 <- rowMeans(normalcount[, treatment_ribo, drop = FALSE], na.rm = TRUE)
  normalcount$logRPFfc <- log2(normalcount$rpf2 / normalcount$rpf1)

  te_sample_names <- character()
  for (index in seq_along(control_rna)) {
    sample_name <- paste0("TE.", control_ribo[[index]], ".", control_rna[[index]])
    normalcount[[sample_name]] <- normalcount[[control_ribo[[index]]]] / normalcount[[control_rna[[index]]]]
    te_sample_names <- c(te_sample_names, sample_name)
  }

  for (index in seq_along(treatment_rna)) {
    sample_name <- paste0("TE.", treatment_ribo[[index]], ".", treatment_rna[[index]])
    normalcount[[sample_name]] <- normalcount[[treatment_ribo[[index]]]] / normalcount[[treatment_rna[[index]]]]
    te_sample_names <- c(te_sample_names, sample_name)
  }

  control_te_names <- paste0("TE.", control_ribo, ".", control_rna)
  treatment_te_names <- paste0("TE.", treatment_ribo, ".", treatment_rna)
  normalcount$TE_A1 <- round(rowMeans(normalcount[, control_te_names, drop = FALSE], na.rm = TRUE), 3)
  normalcount$TE_A2 <- round(rowMeans(normalcount[, treatment_te_names, drop = FALSE], na.rm = TRUE), 3)
  normalcount$logTEfc <- log2(normalcount$TE_A2 / normalcount$TE_A1)

  list(
    normalcount = normalcount,
    te_tool = normalized_parameters$te_tool,
    preprocess_signature = translation_efficiency_preprocess_signature(preprocess_context),
    base_token = translation_efficiency_build_base_token(normalized_parameters$te_tool)
  )
}

translation_efficiency_attach_performance <- function(
  te_context,
  base_context,
  cache_hit = FALSE,
  base_elapsed_sec = NA_real_,
  apply_elapsed_sec = NA_real_
) {
  cached_base_build_elapsed_sec <- if (is.null(base_context$build_elapsed_sec)) {
    NA_real_
  } else {
    unname(as.numeric(base_context$build_elapsed_sec))
  }

  te_context$performance <- list(
    baseToken = base_context$base_token,
    preprocessSignature = base_context$preprocess_signature,
    cacheHit = isTRUE(cache_hit),
    baseElapsedSec = unname(as.numeric(base_elapsed_sec)),
    cachedBaseBuildElapsedSec = cached_base_build_elapsed_sec,
    applyElapsedSec = unname(as.numeric(apply_elapsed_sec)),
    totalElapsedSec = unname(as.numeric(base_elapsed_sec)) + unname(as.numeric(apply_elapsed_sec))
  )
  te_context
}

translation_efficiency_apply_parameters <- function(
  base_context,
  fvalue = 1.5,
  p_cutoff = 0.05,
  p_type = "Fdr"
) {
  stopifnot(is.list(base_context))
  stopifnot(is.data.frame(base_context$normalcount))

  normalized_parameters <- translation_efficiency_normalize_parameters(
    te_tool = base_context$te_tool,
    fvalue = fvalue,
    p_cutoff = p_cutoff,
    p_type = p_type
  )

  normalcount <- base_context$normalcount
  fold_threshold <- log2(max(normalized_parameters$fvalue, 1))

  normalcount$diffExp <- "Non"
  normalcount$diffExp[normalcount$logInputFC >= fold_threshold] <- "Up"
  normalcount$diffExp[normalcount$logInputFC <= -fold_threshold] <- "Down"

  normalcount$diffRibo <- "Non"
  normalcount$diffRibo[normalcount$logRPFfc >= fold_threshold] <- "Up"
  normalcount$diffRibo[normalcount$logRPFfc <= -fold_threshold] <- "Down"

  selected_metric <- if (identical(tolower(as.character(normalized_parameters$p_type)), "rawpvalue")) normalcount$pvalue else normalcount$padj
  normalcount$diffTE <- "Non"
  normalcount$diffTE[normalcount$logTEfc >= fold_threshold & selected_metric < normalized_parameters$p_cutoff] <- "Up"
  normalcount$diffTE[normalcount$logTEfc <= -fold_threshold & selected_metric < normalized_parameters$p_cutoff] <- "Down"

  result_table <- normalcount

  volcano_points <- data.frame(
    GeneID = result_table$GeneID,
    log2FoldChange = result_table$log2FoldChange,
    significance = -log10(pmax(selected_metric, 1e-300)),
    pvalue = result_table$pvalue,
    padj = result_table$padj,
    diffTE = result_table$diffTE,
    stringsAsFactors = FALSE
  )

  status_table <- sort(table(factor(result_table$diffTE, levels = c("Up", "Non", "Down"))), decreasing = FALSE)
  volcano_status <- data.frame(
    status = names(status_table),
    count = as.integer(status_table),
    stringsAsFactors = FALSE
  )

  scatter_rpf <- data.frame(
    GeneID = result_table$GeneID,
    log2InputFC = result_table$logInputFC,
    log2RPFFC = result_table$logRPFfc,
    log2FoldChange = result_table$log2FoldChange,
    diffTE = result_table$diffTE,
    stringsAsFactors = FALSE
  )

  scatter_input <- data.frame(
    GeneID = result_table$GeneID,
    input1 = result_table$input1,
    input2 = result_table$input2,
    log2Input1 = log2(pmax(result_table$input1, 1e-300)),
    log2Input2 = log2(pmax(result_table$input2, 1e-300)),
    diffExp = result_table$diffExp,
    stringsAsFactors = FALSE
  )

  scatter_te_expression <- data.frame(
    GeneID = result_table$GeneID,
    TE_A1 = result_table$TE_A1,
    TE_A2 = result_table$TE_A2,
    log2TE_A1 = log2(pmax(result_table$TE_A1, 1e-300)),
    log2TE_A2 = log2(pmax(result_table$TE_A2, 1e-300)),
    diffTE = result_table$diffTE,
    stringsAsFactors = FALSE
  )

  scatter_te <- data.frame(
    GeneID = result_table$GeneID,
    log2InputFC = result_table$logInputFC,
    log2FoldChange = result_table$log2FoldChange,
    log2RPFFC = result_table$logRPFfc,
    diffTE = result_table$diffTE,
    stringsAsFactors = FALSE
  )

  result_table <- translation_efficiency_result_table_labels(result_table)
  result_table <- translation_efficiency_round_result_table(result_table, digits = 4L)
  result_table <- translation_efficiency_reorder_result_table(result_table)

  list(
    result_table = result_table,
    search_index = translation_efficiency_search_index(result_table),
    volcano_points = volcano_points,
    volcano_status = volcano_status,
    scatter_points = scatter_rpf,
    scatter_input = scatter_input,
    scatter_rpf = scatter_rpf,
    scatter_te_expression = scatter_te_expression,
    scatter_te = scatter_te,
    parameters = list(
      te_tool = normalized_parameters$te_tool,
      fvalue = normalized_parameters$fvalue,
      p_cutoff = normalized_parameters$p_cutoff,
      p_type = normalized_parameters$p_type
    )
  )
}

