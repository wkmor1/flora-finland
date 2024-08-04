unlink("build", TRUE)

dir.create("build")

file.copy("src/styles.css", "build/styles.css", overwrite = TRUE)

viewport <- htmltools::meta(
  name = "viewport", content = "width=device-width, initial-scale=1.0"
)

css <- link(rel = "stylesheet", type = "text/css", href = "/styles.css")

front_page <- with(
  htmltools::tags,
  html(
    head(title("Tracheophyta"), viewport, css),
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
                      stringr::str_to_sentence(
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

htmltools::save_html(front_page, file.path("build", "index.html"))

for (page in list.files("src", recursive = TRUE, pattern = "content.yml")) {

  page_file <- file.path("src", page)

  content <- yaml::yaml.load_file(page_file)

  name <- content[["scientificName"]]

  rank <- content[["taxonRank"]]

  taxon <- dirname(page)

  taxa <- list.dirs(dirname(page_file), recursive = FALSE, full.names = FALSE)

  sp_page <- with(
    htmltools::tags,
    html(
      head(title(name), viewport, css),
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
              img(src = content[["images"]][[1L]][["file"]])
            ),
            div(
              class = "col1",
              # parent taxon
              if (basename(dirname(taxon)) != ".") {
                div(
                  a(
                    "\u2190",
                    span(
                      stringr::str_to_sentence(basename(dirname(taxon))),
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
                if (!is.null(content[["vernacularName"]])) {
                  h2(content[["vernacularName"]], class = "subtitle")
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
                        gsub("_", " ", stringr::str_to_sentence(taxa)),
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
          if (!is.null(content[["description"]])) div(
            class = "row2",
            p(class = "description", content[["description"]])
          )
        )
      )
    )
  )

  dir.create(
    file.path("build", dirname(page)), showWarnings = FALSE, recursive = TRUE
  )

  htmltools::save_html(sp_page, file.path("build", dirname(page), "index.html"))

  file.copy(
    file.path("src", dirname(page), "img0.jpeg"),
    file.path("build", dirname(page), "img0.jpeg"),
    overwrite = TRUE
  )

}
