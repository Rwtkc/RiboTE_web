#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  match_index <- match(flag, args)
  if (is.na(match_index) || match_index >= length(args)) {
    return(default)
  }

  args[[match_index + 1L]]
}

has_flag <- function(flag) {
  flag %in% args
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0 || is.na(lhs) || !nzchar(as.character(lhs))) {
    return(rhs)
  }

  lhs
}

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(...) NULL)
if (!is.null(script_path) && nzchar(script_path)) {
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript new/scripts/build_gene_info_db.R [--species-key hg38] [--gff path] [--out path] [--force]",
      "",
      "Default:",
      "  Builds TEShinyData/orgDB/hg38.geneInfo.sqlite from TEShinyData/gff/hg38.gff.rda.",
      "",
      "The output SQLite database contains a minimal geneInfo table used by",
      "RiboTE Data Preprocess QC: ensembl_gene_id, gene_biotype, and symbol.",
      sep = "\n"
    )
  )
}

if (has_flag("--help") || has_flag("-h")) {
  usage()
  quit(status = 0)
}

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Required package is not installed: %s", package), call. = FALSE)
  }
}

require_package("DBI")
require_package("RSQLite")
require_package("GenomicRanges")
require_package("S4Vectors")

species_key <- arg_value("--species-key", "hg38")
gff_path <- arg_value("--gff", file.path(repo_root, "TEShinyData", "gff", sprintf("%s.gff.rda", species_key)))
out_path <- arg_value("--out", file.path(repo_root, "TEShinyData", "orgDB", sprintf("%s.geneInfo.sqlite", species_key)))
force <- has_flag("--force")

if (!file.exists(gff_path)) {
  stop(sprintf("GFF RDA file does not exist: %s", gff_path), call. = FALSE)
}

if (file.exists(out_path) && !force) {
  stop(sprintf("Output already exists. Use --force to overwrite: %s", out_path), call. = FALSE)
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

strip_ensembl_version <- function(value) {
  sub("\\.[0-9]+$", "", as.character(value))
}

first_existing_column <- function(data_frame, candidates) {
  matched <- candidates[candidates %in% colnames(data_frame)]
  if (length(matched) == 0) {
    return(NULL)
  }

  matched[[1]]
}

cat(sprintf("Loading GFF: %s\n", normalizePath(gff_path, winslash = "/", mustWork = TRUE)))
env <- new.env(parent = emptyenv())
objects <- load(gff_path, envir = env)
if (!"gff" %in% objects) {
  stop("Expected object 'gff' in the RDA file.", call. = FALSE)
}

gff <- get("gff", envir = env)
gff_data <- as.data.frame(gff)

if (!"type" %in% colnames(gff_data) || !"gene_id" %in% colnames(gff_data)) {
  stop("GFF data must contain 'type' and 'gene_id' columns.", call. = FALSE)
}

gene_rows <- gff_data[gff_data$type == "gene" & !is.na(gff_data$gene_id) & nzchar(as.character(gff_data$gene_id)), , drop = FALSE]
if (nrow(gene_rows) == 0) {
  stop("No gene rows were found in the GFF data.", call. = FALSE)
}

biotype_col <- first_existing_column(gene_rows, c("gene_biotype", "gene_type", "biotype"))
symbol_col <- first_existing_column(gene_rows, c("gene_name", "Name", "symbol"))

gene_info <- data.frame(
  ensembl_gene_id = strip_ensembl_version(gene_rows$gene_id),
  gene_biotype = if (!is.null(biotype_col)) as.character(gene_rows[[biotype_col]]) else "Unknown",
  symbol = if (!is.null(symbol_col)) as.character(gene_rows[[symbol_col]]) else NA_character_,
  stringsAsFactors = FALSE
)

gene_info$gene_biotype[is.na(gene_info$gene_biotype) | !nzchar(gene_info$gene_biotype)] <- "Unknown"
gene_info$symbol[is.na(gene_info$symbol) | !nzchar(gene_info$symbol)] <- gene_info$ensembl_gene_id[is.na(gene_info$symbol) | !nzchar(gene_info$symbol)]

gene_info <- gene_info[!duplicated(gene_info$ensembl_gene_id), , drop = FALSE]
gene_info <- gene_info[order(gene_info$ensembl_gene_id), , drop = FALSE]
rownames(gene_info) <- NULL

tmp_path <- sprintf("%s.tmp_%s", out_path, Sys.getpid())
if (file.exists(tmp_path)) {
  unlink(tmp_path)
}

con <- DBI::dbConnect(RSQLite::SQLite(), tmp_path)
on.exit({
  try(DBI::dbDisconnect(con), silent = TRUE)
  if (file.exists(tmp_path)) {
    unlink(tmp_path)
  }
}, add = TRUE)

DBI::dbWriteTable(con, "geneInfo", gene_info, overwrite = TRUE)
invisible(DBI::dbExecute(con, "create unique index idx_geneInfo_ensembl_gene_id on geneInfo(ensembl_gene_id)"))
invisible(DBI::dbExecute(con, "create index idx_geneInfo_gene_biotype on geneInfo(gene_biotype)"))
DBI::dbWriteTable(
  con,
  "metadata",
  data.frame(
    key = c("species_key", "source_gff", "created_at", "gene_rows", "biotype_column", "symbol_column"),
    value = c(
      species_key,
      normalizePath(gff_path, winslash = "/", mustWork = TRUE),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      as.character(nrow(gene_info)),
      biotype_col %||% "",
      symbol_col %||% ""
    ),
    stringsAsFactors = FALSE
  ),
  overwrite = TRUE
)
DBI::dbDisconnect(con)
con <- NULL

if (file.exists(out_path)) {
  unlink(out_path)
}
invisible(file.rename(tmp_path, out_path))

cat(sprintf("Wrote geneInfo rows: %s\n", format(nrow(gene_info), big.mark = ",")))
cat(sprintf("Output DB: %s\n", normalizePath(out_path, winslash = "/", mustWork = TRUE)))
