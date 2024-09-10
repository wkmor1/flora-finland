future::plan(future::multisession, workers = 2)

options(
  finbif_cache_path = "cache",
  finbif_rate_limit = Inf,
  finbif_hide_progress = TRUE
)

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
  filter = list(
    country = "Finland", collection = c("HR.90", "HR.169", "HR.3551", "HR.767")
  ),
  select = c(x = "lon_10_center_ykj", y = "lat_10_center_ykj"),
  aggregate = "records",
  aggregate_counts = FALSE,
  n = "all"
)

p <- sf::st_as_sf(p * 1e4, coords = c("x", "y"), crs = 2393)

p <- sf::st_intersection(p, sf::st_buffer(finland, 5000))

r <- terra::rast(terra::vect(p), res = c(1e4, 1e4))

p <- terra::rasterize(terra::vect(p), r)

gg <-
  ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = p, show.legend = FALSE) +
  ggplot2::scale_fill_continuous(
    low = "sienna", high = "sienna", na.value = "transparent"
  ) +
  ggplot2::geom_sf(
    data = finland, fill = "transparent", color = "darkslategrey"
  ) +
  ggplot2::theme_void()

svglite::svglite(file.path("src", "map.svg"), width = 5, bg = "grey90")

print(gg)

dev.off()

for (page in setdiff(list.dirs("src"), c("src", "src/favicon"))) {

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
    filter  = list(
      country = "Finland",
      collection = c("HR.90", "HR.169", "HR.3551", "HR.767")
    ),
    select = c(x = "lon_10_center_ykj", y = "lat_10_center_ykj"),
    aggregate = "records",
    aggregate_counts = FALSE,
    n = "all"
  )

  p <- sf::st_as_sf(p * 1e4, coords = c("x", "y"), crs = 2393)

  p <- sf::st_intersection(p, sf::st_buffer(finland, 5000))

  p <- terra::rasterize(terra::vect(p), r)

  gg <-
    ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = p, show.legend = FALSE) +
    ggplot2::scale_fill_continuous(
      low = "sienna", high = "sienna", na.value = "transparent"
    ) +
    ggplot2::geom_sf(
      data = finland, fill = "transparent", color = "darkslategrey"
    ) +
    ggplot2::theme_void()

  svglite::svglite(file.path(page, "map.svg"), width = 5, bg = "grey90")

  print(gg)

  dev.off()

}
