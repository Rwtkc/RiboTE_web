library(shiny)

shared_source <- paste(
  readLines("new/modules/load_data/load_data.shared.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")

species_choices <- ribote_species_choices()
controls_config <- ribote_load_data_controls_config("load_data")
human_meta <- ribote_species_meta("Homo sapiens (hg38)")

stopifnot(identical(species_choices, "Homo sapiens (hg38)"))
stopifnot(is.list(controls_config$species_choices))
stopifnot(identical(unlist(controls_config$species_choices, use.names = FALSE), species_choices))
stopifnot(is.list(human_meta))
stopifnot(identical(human_meta$acronym, "hg38"))
stopifnot(identical(human_meta$org_id, 99L))
stopifnot(identical(human_meta$org_db_name, "hg38.geneInfo.sqlite"))
stopifnot(!grepl("RiboTE_Org.csv", shared_source, fixed = TRUE))

cat("ribote load data species registry test passed\n")
