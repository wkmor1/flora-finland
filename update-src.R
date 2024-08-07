options(finbif_cache_path = "cache", finbif_rate_limit = Inf)

checklist <- read.csv("checklist.csv")

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

finland <- readRDS("finland.rds")

p <- finbif::finbif_occurrence(
  "Tracheophyta",
  filter = c(country = "Finland", collection = "HR.90"),
  select = c(x = "lon_50_center_ykj", y = "lat_50_center_ykj"),
  aggregate = "records",
  aggregate_counts = FALSE,
  n = "all"
)

p <- transform(p, x = ifelse(y %% 2, x + 2.5, x))

p <- sf::st_as_sf(p * 1e4, coords = c("x", "y"), crs = 2393)

p <- sf::st_transform(p, crs = 3067)

gg <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = p, shape = "\u2b22", size = 9, color = "grey30") +
  ggplot2::geom_sf(data = finland, fill = "grey90") +
  ggplot2::coord_sf(
    xlim = c(60000, 760000), ylim = c(6600000, 7800000), expand = FALSE
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey90"))

svglite::svglite(file.path("src", "map.svg"), width = 5, bg = "grey90")

print(gg)

dev.off()

for (page in list.dirs("src")[-1]) {

  taxon <- basename(page)

  content_file <- file.path(page, "content.yml")

  if (!file.exists(content_file)) file.create(content_file)

  content <- yaml::yaml.load_file(content_file, readLines.warn = FALSE)

  checklist_taxon <-subset(
    checklist, gsub(" ", "_", tolower(scientificName)) == taxon
  )

  content[["finbifID"]] <- checklist_taxon[["taxonID"]]

  if (!length(content[["finbifID"]])) {

    content[["finbifID"]] <- finbif::taxon_id(taxon)

  }

  taxon_data <- finbif::finbif_taxa(content[["finbifID"]])
  taxon_data <- taxon_data[["content"]]
  taxon_data <- taxon_data[[1L]]

  content[["scientificName"]] <- taxon_data[["scientificName"]]

  content[["taxonRank"]] <- sub("MX.", "", taxon_data[["taxonRank"]])

  yaml::write_yaml(content, content_file)

  p <- finbif::finbif_occurrence(
    content[["finbifID"]],
    filter = c(country = "Finland", collection = "HR.90"),
    select = c(x = "lon_50_center_ykj", y = "lat_50_center_ykj"),
    aggregate = "records",
    aggregate_counts = FALSE,
    n = "all"
  )

  p <- transform(p, x = ifelse(y %% 2, x + 2.5, x))

  p <- sf::st_as_sf(p * 1e4, coords = c("x", "y"), crs = 2393)

  p <- sf::st_transform(p, crs = 3067)

  gg <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = p, shape = "\u2b22", size = 9, color = "grey30") +
    ggplot2::geom_sf(data = finland, fill = "grey90") +
    ggplot2::coord_sf(
      xlim = c(60000, 760000), ylim = c(6600000, 7800000), expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey90"))

  svglite::svglite(file.path(page, "map.svg"), width = 5, bg = "grey90")

  print(gg)

  dev.off()

}
