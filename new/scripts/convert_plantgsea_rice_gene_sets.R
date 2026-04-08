#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x)) y else x
}

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit) || hit[length(hit)] >= length(args)) {
    return(default)
  }
  args[[hit[length(hit)] + 1L]]
}

has_flag <- function(flag) {
  flag %in% args
}

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1] %||% "new/scripts/convert_plantgsea_rice_gene_sets.R")
repo_root <- normalizePath(file.path(dirname(script_file), "..", ".."), mustWork = FALSE)
if (!dir.exists(file.path(repo_root, "new"))) {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
}

go_path <- get_arg("--go", file.path(repo_root, "new", "Osa_GO.txt"))
kegg_path <- get_arg("--kegg", file.path(repo_root, "new", "Osa_KEGG.txt"))
out_dir <- get_arg("--out-dir", file.path(repo_root, "new", "resources", "gene_sets", "plantgsea", "osa_IRGSP_1"))
riceidconverter_tar <- get_arg("--riceidconverter-tar", NULL)

if (!file.exists(go_path) && file.exists(file.path(out_dir, "source", "Osa_GO.txt"))) {
  go_path <- file.path(out_dir, "source", "Osa_GO.txt")
}
if (!file.exists(kegg_path) && file.exists(file.path(out_dir, "source", "Osa_KEGG.txt"))) {
  kegg_path <- file.path(out_dir, "source", "Osa_KEGG.txt")
}

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " not found: ", path, call. = FALSE)
  }
}

sanitize_field <- function(x) {
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(x)
}

load_riceidconverter_db <- function(tar_path = NULL) {
  if (requireNamespace("riceidconverter", quietly = TRUE)) {
    ns <- asNamespace("riceidconverter")
    if (exists("riceiddb", envir = ns, inherits = FALSE)) {
      return(get("riceiddb", envir = ns))
    }
  }

  if (is.null(tar_path)) {
    tar_path <- file.path(tempdir(), "riceidconverter_1.1.1.tar.gz")
    if (!file.exists(tar_path)) {
      utils::download.file(
        "https://cran.r-project.org/src/contrib/riceidconverter_1.1.1.tar.gz",
        tar_path,
        mode = "wb",
        quiet = TRUE
      )
    }
  }
  stop_if_missing(tar_path, "riceidconverter source tarball")

  extract_dir <- file.path(tempdir(), "riceidconverter_src")
  if (dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::untar(tar_path, files = "riceidconverter/R/sysdata.rda", exdir = extract_dir)

  sysdata <- file.path(extract_dir, "riceidconverter", "R", "sysdata.rda")
  stop_if_missing(sysdata, "riceidconverter sysdata")
  env <- new.env(parent = emptyenv())
  load(sysdata, envir = env)
  if (!exists("riceiddb", envir = env, inherits = FALSE)) {
    stop("riceidconverter sysdata does not contain riceiddb", call. = FALSE)
  }
  get("riceiddb", envir = env)
}

build_msu_to_rap_map <- function(riceiddb) {
  required <- c("RAP", "MSU")
  if (!all(required %in% names(riceiddb))) {
    stop("riceiddb must contain columns: ", paste(required, collapse = ", "), call. = FALSE)
  }
  rap <- as.character(riceiddb$RAP)
  msu <- as.character(riceiddb$MSU)
  keep <- !is.na(rap) & rap != "None" &
    grepl("^Os[0-9][0-9]g[0-9]+$", rap) &
    !is.na(msu) & msu != "None"
  rap <- rap[keep]
  msu <- sub("\\.[0-9]+$", "", msu[keep])
  mapping_df <- unique(data.frame(msu = msu, rap = rap, stringsAsFactors = FALSE))
  split(mapping_df$rap, mapping_df$msu)
}

read_plantgsea <- function(path) {
  stop_if_missing(path, "PlantGSEA input")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  parts <- strsplit(lines, "\t", fixed = TRUE)
  bad <- which(lengths(parts) < 3L)
  if (length(bad)) {
    stop("Invalid PlantGSEA row with fewer than 3 tab-separated fields at line ", bad[[1]], call. = FALSE)
  }
  data.frame(
    term = vapply(parts, `[[`, character(1), 1L),
    description = vapply(parts, `[[`, character(1), 2L),
    genes = vapply(parts, function(x) paste(x[-seq_len(2L)], collapse = ","), character(1)),
    stringsAsFactors = FALSE
  )
}

convert_rows <- function(rows, mapping, min_size = 1L) {
  converted <- vector("list", nrow(rows))
  unmapped <- integer(nrow(rows))
  original_sizes <- integer(nrow(rows))
  converted_sizes <- integer(nrow(rows))

  for (i in seq_len(nrow(rows))) {
    genes <- trimws(unlist(strsplit(rows$genes[[i]], ",", fixed = TRUE), use.names = FALSE))
    genes <- unique(genes[nzchar(genes)])
    original_sizes[[i]] <- length(genes)
    mapped <- unlist(mapping[genes], use.names = FALSE)
    direct_rap <- genes[grepl("^Os[0-9][0-9]g[0-9]+$", genes)]
    mapped <- unique(c(mapped, direct_rap))
    mapped <- mapped[!is.na(mapped) & grepl("^Os[0-9][0-9]g[0-9]+$", mapped)]
    converted_sizes[[i]] <- length(mapped)
    unmapped[[i]] <- sum(!genes %in% names(mapping) & !genes %in% direct_rap)
    converted[[i]] <- mapped
  }

  keep <- converted_sizes >= min_size
  list(
    rows = rows[keep, c("term", "description"), drop = FALSE],
    genes = converted[keep],
    original_sizes = original_sizes[keep],
    converted_sizes = converted_sizes[keep],
    unmapped = unmapped[keep],
    dropped = sum(!keep)
  )
}

write_gmt <- function(converted, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- character(nrow(converted$rows))
  for (i in seq_len(nrow(converted$rows))) {
    lines[[i]] <- paste(
      c(
        sanitize_field(converted$rows$term[[i]]),
        sanitize_field(converted$rows$description[[i]]),
        converted$genes[[i]]
      ),
      collapse = "\t"
    )
  }
  writeLines(lines, path, useBytes = TRUE)
}

split_go_category <- function(description) {
  desc <- tolower(description)
  ifelse(
    grepl("goslim:biological_process", desc, fixed = TRUE),
    "bp",
    ifelse(
      grepl("goslim:molecular_function", desc, fixed = TRUE),
      "mf",
      ifelse(grepl("goslim:cellular_component", desc, fixed = TRUE), "cc", NA_character_)
    )
  )
}

summarize_converted <- function(label, converted) {
  total_genes <- length(unique(unlist(converted$genes, use.names = FALSE)))
  cat(
    label, ": sets=", nrow(converted$rows),
    ", unique RAP genes=", total_genes,
    ", dropped empty sets=", converted$dropped,
    ", median converted set size=", stats::median(converted$converted_sizes),
    "\n",
    sep = ""
  )
}

stop_if_missing(go_path, "Osa_GO.txt")
stop_if_missing(kegg_path, "Osa_KEGG.txt")

riceiddb <- load_riceidconverter_db(riceidconverter_tar)
mapping <- build_msu_to_rap_map(riceiddb)
cat("MSU->RAP mapping keys: ", length(mapping), "\n", sep = "")

go_rows <- read_plantgsea(go_path)
go_rows$category <- split_go_category(go_rows$description)
if (anyNA(go_rows$category)) {
  warning(sum(is.na(go_rows$category)), " GO rows did not match BP/MF/CC category and will be skipped.")
}

go_outputs <- list(
  bp = file.path(out_dir, "osa_IRGSP_1.go.bp.gmt"),
  mf = file.path(out_dir, "osa_IRGSP_1.go.mf.gmt"),
  cc = file.path(out_dir, "osa_IRGSP_1.go.cc.gmt")
)

for (category in names(go_outputs)) {
  subset_rows <- go_rows[go_rows$category == category, c("term", "description", "genes"), drop = FALSE]
  converted <- convert_rows(subset_rows, mapping)
  write_gmt(converted, go_outputs[[category]])
  summarize_converted(paste0("GO ", toupper(category)), converted)
  cat("  wrote ", normalizePath(go_outputs[[category]], winslash = "/", mustWork = FALSE), "\n", sep = "")
}

kegg_rows <- read_plantgsea(kegg_path)
kegg_converted <- convert_rows(kegg_rows, mapping)
kegg_output <- file.path(out_dir, "osa_IRGSP_1.kegg.gmt")
write_gmt(kegg_converted, kegg_output)
summarize_converted("KEGG", kegg_converted)
cat("  wrote ", normalizePath(kegg_output, winslash = "/", mustWork = FALSE), "\n", sep = "")

if (has_flag("--delete-source")) {
  unlink(c(go_path, kegg_path), force = TRUE)
}
