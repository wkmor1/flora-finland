library(yaml)
library(htmltools)
library(finbif)

options(finbif_cache_path = "cache", finbif_rate_limit = Inf)

for (page in list.dirs("src", recursive = TRUE)[-1]) {

  content_file <- file.path(page, "content.yml")

  if (!file.exists(content_file)) file.create(content_file)

  content <- yaml.load_file(content_file,readLines.warn = FALSE)

  content$finbifID <- basename(
    subset(checklist, scientificName == basename(page))$taxonID
  )

  if (!length(content$finbifID)) {

    content$finbifID <- taxon_id(basename(page))

  }

  write_yaml(content, content_file)

}

file.copy("src/styles.css", "build/styles.css", overwrite = TRUE)

for (page in list.files("src", recursive = TRUE, pattern = "content.yml")) {

  page_file <- file.path("src", page)

  content <- yaml.load_file(page_file)

  name <- scientific_name(content$finbifID)

  taxon <- dirname(page_file)

  parent <- basename(dirname(taxon))

  taxa <- list.dirs(taxon, recursive = FALSE, full.names = FALSE)

  sp_page <- with(
    tags,
    html(
      head(
        title(name),
        link(rel = "stylesheet", type = "text/css", href = "/styles.css")
      ),
      body(
        div(
          div(
            div(
              h1("Flora of Finland")
            ),
            if (length(taxa)) div(
              do.call(
                ul,
                lapply(
                  mapply(
                    a,
                    taxa,
                    href = paste0(taxa, "/"),
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE
                  ),
                  li
                )
              )
            ),
            div(
              img(src = content$images[[1L]][["file"]], width = 500)
            )
          ),
          if (parent != "build") div(
            h3(parent)
          ),
          div(
            h2(name)
          ),
          div(
            h2("Description"),
            p(content$description)
          )
        )
      )
    )
  )

  dir.create(
    file.path("build", dirname(page)), showWarnings = FALSE, recursive = TRUE
  )

  save_html(sp_page, file.path("build", dirname(page), "index.html"))

  file.copy(
    file.path("src", dirname(page), "img0.jpeg"),
    file.path("build", dirname(page), "img0.jpeg"),
    overwrite = TRUE
  )

}
