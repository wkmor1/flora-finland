unlink("build", TRUE)

dir.create("build")

file.copy("src/styles.css", "build/styles.css", overwrite = TRUE)

file.copy("src/img0.jpeg", "build/img0.jpeg", overwrite = TRUE)

file.copy("src/map.svg", "build/map.svg", overwrite = TRUE)

ranks <- list(
  class   = c(plural = "classes:",  child = "order",      parent = "phylum"),
  order   = c(plural = "orders:",   child = "family",     parent = "class"),
  family  = c(plural = "families:", child = "genus",      parent = "order"),
  genus   = c(plural = "genera:",   child = "species",    parent = "family"),
  species = c(plural = "species:",  child = "subspecies", parent = "genus")
)

map_source <- file.path(
  "https://laji.fi",
  "en",
  "observation",
  "finnish?target=%s&countryId=ML.206&collectionId=HR.90,HR.169,HR.3551,HR.767"
)

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

page_footer <- htmltools::withTags(
  footer(
    class = "main-footer",
    p(
      class = "license",
      `xmlns:cc` = "http://creativecommons.org/ns#",
      `xmlns:dct` = "http://purl.org/dc/terms/",
      "This website is marked with",
      a(
        class = "license-link",
        href = "https://creativecommons.org/publicdomain/zero/1.0/",
        target = "_blank",
        rel = "license noopener noreferrer",
        "CC0 1.0 Universal",
        img(
          class = "license-image",
          src = "https://mirrors.creativecommons.org/presskit/icons/cc.svg",
          alt = ""
        ),
        img(
          class = "license-image",
          src = "https://mirrors.creativecommons.org/presskit/icons/zero.svg",
          alt = ""
        )
      )
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
                class = "figure",
                img(
                  class = "main-img",
                  src = "/img0.jpeg",
                  alt = "Pitkäjärvi, Enontekiö"
                ),
                figcaption(
                  class = "info",
                  details(
                    class = "info-content",
                    summary(
                      class = "info-button",
                      htmltools::HTML("&#9432;")
                    ),
                    p(
                      class = "info-text",
                      "Pitkäjärvi, Enontekiö"
                    )
                  )
                )
              )
            ),
            div(
              class = "col1",
              # taxon name
              div(
                span("phylum", class = "rank"),
                h1(
                  class = "page-title",
                  "Tracheophyta"
                ),
                h2("Vascular plants", class = "subtitle")
              ),
              # child taxa
              div(
                h3(
                  class = "taxa-list-title",
                  "Classes"
                ),
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
          ),
          div(
            class = "row2",
            article(
              class = "col2",
              p(
                class = "description",
                "Plants with vascular tissue and dominant sporophyte generations."
              ),
              div(
                class = "info",
                details(
                  class = "info-content",
                  summary(
                    class = "info-button",
                    htmltools::HTML("&#9432;")
                  ),
                  p(
                    class = "info-text",
                    "Sources"
                  )
                )
              )
            ),
            div(
              class = "col1",
              figure(
                class = "figure",
                img(class = "map", src = "map.svg"),
                figcaption(
                  class = "info",
                  details(
                    class = "info-content",
                    summary(
                      class = "info-button",
                      htmltools::HTML("&#9432;")
                    ),
                    p(
                      class = "info-text",
                      a(
                        "Source",
                        href = sprintf(map_source, "MX.53078")
                      )
                    )
                  )
                )
              )
            )
          ),
          page_footer
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
                if (!is.null(content[["images"]])) figure(
                  class = "figure",
                  img(
                    class = "main-img",
                    src = content[["images"]][[1L]][["file"]],
                    alt = content[["images"]][[1L]][["alt"]]
                  ),
                  figcaption(
                    class = "info",
                    details(
                      class = "info-content",
                      summary(
                        class = "info-button",
                        htmltools::HTML("&#9432;")
                      ),
                      p(
                        class = "info-text",
                        htmltools::HTML(content[["images"]][[1L]][["caption"]]),
                        " | ",
                        a(
                          "Source",
                          href = content[["images"]][[1L]][["src"]]
                        )
                      )
                    )
                  )
                )
              ),
              div(
                class = "col1",
                # parent taxon
                if (basename(dirname(taxon)) != ".") {
                  div(
                    a(
                      class = "nav-parent",
                      "\u2190",
                      span(
                        class = "parent-taxa-rank",
                        sprintf("%s:", ranks[[c(rank, "parent")]])
                      ),
                      span(
                        stringr::str_to_sentence(basename(dirname(taxon))),
                        class = ranks[[c(rank, "parent")]]
                      ),
                      href = file.path("", dirname(taxon))
                    )
                  )
                } else {
                  div(
                    a(
                      class = "nav-parent",
                      "\u2190",
                      span(
                        class = "parent-taxa-rank",
                        "phylum:"
                      ),
                      span("Tracheophyta", class ="phylum"),
                      href = "/"
                    )
                  )
                },
                # taxon name
                div(
                  if (rank != "species") span(rank, class = "rank"),
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
                  h3(
                    class = "taxa-list-title",
                    ranks[[c(ranks[[c(rank, "child")]], "plural")]]
                  ),
                  ul(
                    lapply(
                      mapply(
                        a,
                        lapply(
                          gsub("_", " ", stringr::str_to_sentence(taxa)),
                          span,
                          class = ranks[[c(rank, "child")]]
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
            div(
              class = "row2",
              article(
                class = "col2",
                if (!is.null(content[["description"]])) {
                  p(class = "description", content[["description"]])
                }
              ),
              div(
                class = "col1",
                figure(
                  class = "figure",
                  img(class = "map", src = "map.svg"),
                  figcaption(
                    class = "info",
                    details(
                      class = "info-content",
                      summary(
                        class = "info-button",
                        htmltools::HTML("&#9432;")
                      ),
                      p(
                        class = "info-text",
                        a(
                          "Source",
                          href = sprintf(
                            map_source, content[["finbifID"]]
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            page_footer
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

  file.copy(
    file.path("src", taxon, "map.svg"),
    file.path("build", taxon, "map.svg"),
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
        ),
        page_footer
      )
    )
  )
)

dir.create(
  file.path("build", "taxa-index"), showWarnings = FALSE, recursive = TRUE
)

htmltools::save_html(index_page, file.path("build", "taxa-index/index.html"))
