unlink("build", TRUE)

dir.create("build")

file.copy("src/styles.css", "build/styles.css", overwrite = TRUE)

file.copy("src/img0.jpeg", "build/img0.jpeg", overwrite = TRUE)

ranks <- c("class", "order", "family", "genus", "species")

child_ranks <- setNames(ranks[-1], ranks[-5])

parent_ranks <- setNames(ranks[-5], ranks[-1])

viewport <- htmltools::withTags(
  meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
)

css <- htmltools::withTags(
  link(rel = "stylesheet", type = "text/css", href = "/styles.css")
)

nav_bar <- htmltools::withTags(
  header(
    class = "main-header",
    nav(
      class = "navbar",
      a(href = "/", class = "nav-home", "Flora of Finland"),
      a(href ="/taxa-index", class = "nav-link", "Index")
    )
  )
)

index <- list()

# front page
front_page <- htmltools::withTags(
  htmltools::tagList(
    html(
      head(title("Flora of Finland"), viewport, css),
      body(
        main(
          class = "main",
          nav_bar,
          div(
            class = "row1",
            # image
            div(
              class = "col2",
              figure(
                class = "main-figure",
                img(
                  class = "main-img", src = "/img0.jpeg",
                  alt = "Pitkäjärvi, Enontekiö"
                ),
                figcaption(
                  class = "main-figcaption",
                  details(
                    class = "main-figcaption-content",
                    summary(
                      class = "main-figcaption-button",
                      htmltools::HTML("&#x1F6C8;")
                    ),
                    p(
                      class = "main-figcaption-text", "Pitkäjärvi, Enontekiö"
                    )
                  )
                )
              )
            ),
            div(
              class = "col1",
              # child taxa
              div(
                ul(
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
)

htmltools::save_html(front_page, file.path("build", "index.html"))

# taxon pages
for (page in list.files("src", recursive = TRUE, pattern = "content.yml")) {

  taxon <- dirname(page)

  index[["page"]] <-  c(index[["page"]], taxon)

  page_file <- file.path("src", page)

  content <- yaml::yaml.load_file(page_file)

  name <- content[["scientificName"]]

  index[["name"]] <-  c(index[["name"]], name)

  rank <- content[["taxonRank"]]

  index[["rank"]] <-  c(index[["rank"]], rank)

  page_dir <- dirname(page_file)

  siblings <- sub(
    "./",
    "",
    file.path(
      dirname(taxon), list.dirs(dirname(page_dir), FALSE, FALSE)
    ),
    fixed = TRUE
  )

  prev_taxon <- siblings[which(siblings == taxon) - 1L]

  next_taxon <- siblings[which(siblings == taxon) + 1L]

  taxa <- list.dirs(page_dir, recursive = FALSE, full.names = FALSE)

  taxon_page <- htmltools::withTags(
    htmltools::tagList(
      html(
        head(title(name), viewport, css),
        body(
          main(
            class = "main",
            nav_bar,
            div(
              class = "row1",
              # image
              div(
                class = "col2",
                img(
                  class = "main-img",
                  src = content[["images"]][[1L]][["file"]]
                )
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
                    a(
                      "\u2190",
                      span("Tracheophyta", class ="phylum"),
                      href = "/"
                    )
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
                # prev or next
                div(
                  if (length(prev_taxon)) {

                    a(
                      class = "prev",
                      "\u25C2 Prev",
                      href = file.path("", prev_taxon)
                    )

                  },
                  if (!is.na(next_taxon)) {

                    a(
                      class = "next",
                      "Next \u25B8",
                      href = file.path("", next_taxon)
                    )

                  }
                ),
                # child taxa
                if (length(taxa)) div(
                  ul(
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
            if (!is.null(content[["description"]])) article(
              class = "row2",
              p(class = "description", content[["description"]])
            )
          )
        )
      )
    )
  )

  dir.create(
    file.path("build", taxon), showWarnings = FALSE, recursive = TRUE
  )

  htmltools::save_html(taxon_page, file.path("build", taxon, "index.html"))

  file.copy(
    file.path("src", taxon, "img0.jpeg"),
    file.path("build", taxon, "img0.jpeg"),
    overwrite = TRUE
  )

}

# index
index <- as.data.frame(index)

index <- index[order(index[["name"]]), ]

index[["letter"]] <- substr(index[["name"]], 1, 1)

index <- split(index, index[["letter"]])

index_page <- htmltools::withTags(
  htmltools::tagList(
    head(title("Index"), viewport, css),
    body(
      main(
        class = "main",
        nav_bar,
        div(
          class = "index-container",
          ul(
            class = "index",
            mapply(
              li,
              lapply(names(index), h3),
              lapply(
                lapply(
                  lapply(
                    index,
                    \(x) {
                      name <- mapply(
                        span,
                        x[["name"]],
                        class = x[["rank"]],
                        SIMPLIFY = FALSE,
                        USE.NAMES = FALSE
                      )
                      mapply(
                        a,
                        name,
                        href = file.path("", x[["page"]]),
                        SIMPLIFY = FALSE,
                        USE.NAMES = FALSE
                      )
                    }
                  ),
                  lapply,
                  li
                ),
                ul,
                class = "index"
              ),
              SIMPLIFY = FALSE,
              USE.NAMES = FALSE
            )
          )
        )
      )
    )
  )
)

dir.create(
  file.path("build", "taxa-index"), showWarnings = FALSE, recursive = TRUE
)

htmltools::save_html(index_page, file.path("build", "taxa-index/index.html"))
