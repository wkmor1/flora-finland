options(finbif_cache_path = "cache", finbif_rate_limit = Inf)

checklist <- read.csv("checklist.csv")

ranks <- c("class", "order", "family", "genus", "species")

child_ranks <- setNames(ranks[-1], ranks[-5])

parent_ranks <- setNames(ranks[-5], ranks[-1])

paths <- with(
  checklist,
  gsub(
    " ",
    "_",
    tolower(file.path("src", class, order, family, genus, scientificName))
  )
)

for (path in paths) {

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

}

for (page in list.dirs("src")[-1]) {

  content_file <- file.path(page, "content.yml")

  if (!file.exists(content_file)) file.create(content_file)

  content <- yaml::yaml.load_file(content_file, readLines.warn = FALSE)

  checklist_taxon <-subset(
    checklist, gsub(" ", "_", tolower(scientificName)) == basename(page)
  )

  content[["finbifID"]] <- checklist_taxon[["taxonID"]]

  if (!length(content[["finbifID"]])) {

    content[["finbifID"]] <- finbif::taxon_id(basename(page))

  }

  taxon_data <- finbif::finbif_taxa(content[["finbifID"]])
  taxon_data <- taxon_data[["content"]]
  taxon_data <- taxon_data[[1L]]

  content[["scientificName"]] <- taxon_data[["scientificName"]]

  content[["taxonRank"]] <- sub("MX.", "", taxon_data[["taxonRank"]])

  yaml::write_yaml(content, content_file)

}
