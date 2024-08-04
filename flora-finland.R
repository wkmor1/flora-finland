library(yaml)
library(htmltools)
library(finbif)
library(stringr)

options(finbif_cache_path = "cache", finbif_rate_limit = Inf)

checklist <- readRDS("checklist.rds")

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

  content <- yaml.load_file(content_file, readLines.warn = FALSE)

  content$finbifID <- basename(
    subset(
      checklist,
      gsub(" ", "_", tolower(scientificName)) == basename(page)
    )$taxonID
  )

  if (!length(content$finbifID)) {

    content$finbifID <- taxon_id(basename(page))

  }

  write_yaml(content, content_file)

}

unlink("build", TRUE)

dir.create("build")

file.copy("src/styles.css", "build/styles.css", overwrite = TRUE)

front_page <- with(
  tags,
  html(
    head(
      title("Tracheophyta"),
      meta(
        name = "viewport", content = "width=device-width, initial-scale=1.0"
      ),
      link(rel = "stylesheet", type = "text/css", href = "/styles.css")
    ),
    body(
      # container
      div(
        class = "main",
        # page title
        div(
          class = "navbar-heading",
          span("Flora of Finland")
        ),
        div(
          class = "row1",
          div(
            class = "col1",
            # child taxa
            div(
              do.call(
                ul,
                lapply(
                  mapply(
                    a,
                    lapply(
                      str_to_sentence(
                        basename(list.dirs("src", recursive = FALSE))
                      ),
                      span,
                      class = "class"
                    ),
                    href = paste0(
                      basename(list.dirs("src", recursive = FALSE)), "/"
                    ),
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE
                  ),
                  li
                )
              )
            )
          )
        )
      )
    )
  )
)

save_html(front_page, file.path("build", "index.html"))

for (page in list.files("src", recursive = TRUE, pattern = "content.yml")) {

  page_file <- file.path("src", page)

  content <- yaml.load_file(page_file)

  taxon_data <- finbif_taxa(content[["finbifID"]])[["content"]][[1L]]

  name <- taxon_data$scientificName

  rank <- sub("MX.", "", taxon_data$taxonRank)

  taxon <- dirname(page)

  taxa <- list.dirs(dirname(page_file), recursive = FALSE, full.names = FALSE)

  sp_page <- with(
    tags,
    html(
      head(
        title(name),
        meta(
          name = "viewport", content = "width=device-width, initial-scale=1.0"
        ),
        link(rel = "stylesheet", type = "text/css", href = "/styles.css")
      ),
      body(
        # container
        div(
          class = "main",
          # page title
          div(
            class = "navbar-heading",
            span("Flora of Finland")
          ),
          div(
            class = "row1",
            # image
            div(
              class = "col2",
              img(src = content$images[[1L]][["file"]])
            ),
            div(
              class = "col1",
              # parent taxon
              if (basename(dirname(taxon)) != ".") {
                div(
                  a(
                    "\u2190",
                    span(
                      str_to_sentence(basename(dirname(taxon))),
                      class = parent_ranks[[rank]]
                    ),
                    href = file.path("", dirname(taxon))
                  )
                )
              } else {
                div(
                  a("\u2190", span("Tracheophyta", class ="phylum"), href = "/")
                )
              },
              # taxon name
              div(
                h1(
                  class = "page-title",
                  span(name, class = rank)
                ),
                if (!is.null(content$vernacularName)) {
                  h2(content$vernacularName, class = "subtitle")
                }
              ),
              # child taxa
              if (length(taxa)) div(
                do.call(
                  ul,
                  lapply(
                    mapply(
                      a,
                      lapply(
                        gsub("_", " ", str_to_sentence(taxa)),
                        span,
                        class = child_ranks[[rank]]
                      ),
                      href = paste0(taxa, "/"),
                      SIMPLIFY = FALSE,
                      USE.NAMES = FALSE
                    ),
                    li
                  )
                )
              )
            )
          ),
          # description
          if (!is.null(content$description)) div(
            class = "row2",
            p(class = "description", content$description)
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
