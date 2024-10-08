unlink("build", TRUE)

dir.create("build")

file.copy("src/styles.css", "build/styles.css", overwrite = TRUE)

file.copy("src/img0.jpeg", "build/img0.jpeg", overwrite = TRUE)

file.copy("src/map.svg", "build/map.svg", overwrite = TRUE)

file.copy(
  list.files("src/favicon", full.names = TRUE), "build", overwrite = TRUE
)

ranks <- list(
  phylum  = c(plural = "phyla:",    child = "class",      parent = "kingdom"),
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

link2glossary <- function(text, path = "/glossary") {

  matches <- regmatches(text, gregexpr("_(.*?)_", text))[[1L]]

  terms <- gsub("_", "", matches)

  links <- mapply(
    \(x, y) regmatches(x, y)[[1L]],
    terms,
    lapply(terms, \(z) gregexpr("\\[(.*?)\\]", z)),
    USE.NAMES = FALSE,
    SIMPLIFY = FALSE
  )

  terms <- mapply(
    \(x, y) if (length(x)) sub(x, "", y, fixed = TRUE) else y,
    links,
    terms,
    USE.NAMES = TRUE,
    SIMPLIFY = FALSE
  )

  links <- mapply(
    \(x, y) if (length(x)) gsub("\\[|\\]", "", x) else y,
    links,
    terms,
    USE.NAMES = FALSE,
    SIMPLIFY = FALSE
  )

  for (i in seq_along(matches)) {

    text <- sub(
      matches[[i]],
      sprintf(
        "<a href=\"%s#%s\">%s</a>",
        path,
        gsub(" ", "-", links[[i]]),
        terms[[i]]
      ),
      text,
      fixed = TRUE
    )

  }

  htmltools::HTML(text)

}

description <- "An online flora of Finland"

meta_data <- htmltools::withTags(
  htmltools::tagList(
    meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    meta(name = "description", content = description),
    meta(name = "keywords", content = "flora,Finland,botany"),
    link(rel = "schema.DC", href = "http://purl.org/DC/elements/1.0/"),
    meta(name = "DC.Title", content = "Flora of Finland"),
    meta(name = "DC.Description", content = description),
    meta(name = "DC.Date", content = Sys.Date()),
    meta(name = "DC.Language", content = "en"),
    meta(name = "DC.Subject", content = "flora")
  )
)

css <- htmltools::withTags(
  link(rel = "stylesheet", type = "text/css", href = "/styles.css")
)

favicon <- htmltools::withTags(
  htmltools::tagList(
    link(
      rel = "apple-touch-icon",
      sizes = "180x180",
      href = "/apple-touch-icon.png"
    ),
    link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "/favicon-32x32.png"
    ),
    link(
      rel = "icon",
      type = "image/png",
      sizes = "16x16",
      href = "/favicon-16x16.png"
    ),
    link(
      rel = "manifest",
      href = "/site.webmanifest"
    )
  )
)

nav_bar <- htmltools::withTags(
  header(
    class = "main-header",
    nav(
      class = "navbar",
      a(href = "/", class = "nav-home", "Flora of Finland"),
      div(
        class = "nav-links",
        a(href = "#", tabindex = "0", class = "dropdown", "☰"),
        ul(
          class = "nav-link-list",
          li(
            class = "nav-link",
            a(href ="/glossary", tabindex = "0", "Glossary")
          ),
          li(
            class = "nav-link",
            a(href ="/taxa-index", tabindex = "0", "Index")
          )
        )
      ),
      div(
        class = "nav-links-dismiss",
        a(href = "#", tabindex = "-1", `aria-hidden` = "true", "☰")
      )
    )
  )
)

page_footer <- htmltools::withTags(
  footer(
    class = "main-footer",
    p(
      class = "license",
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
          alt = "Creative commons icon"
        ),
        img(
          class = "license-image",
          src = "https://mirrors.creativecommons.org/presskit/icons/zero.svg",
          alt = "Creative commons zero icon"
        )
      )
    )
  )
)

image_constructor <- function(x) {

  htmltools::withTags(
    figure(
      class = "figure",
      role = "group",
      img(class = "main-img", src = x[["file"]], alt = x[["alt"]]),
      figcaption(
        class = "info",
        details(
          class = "info-content",
          summary(class = "info-button", htmltools::HTML("&#9432;")),
          p(
            class = "info-text",
            htmltools::HTML(x[["caption"]]),
            " | ",
            a("Source", href = x[["src"]])
          )
        )
      )
    )
  )

}

images <- function(content) {

  imgs <- content[["images"]]

  n_imgs <- length(imgs)

  if (n_imgs > 1) {

    htmltools::withTags(
      section(
        class = "carousel",
        `aria-label`= "Gallery",
        div(
          class = "carousel-viewport",
          tabindex = "-1",
          lapply(
            seq_len(n_imgs),
            function(i) {

              goto <- "Go to %s image"

              prv_n <- i - 1
              nxt_n <- i + 1
              prv <- "previous"
              nxt <- "next"

              if (i == 1) {

                prv_n <- n_imgs
                prv <- "last"

              }

              if (i == n_imgs) {

                nxt_n <- 1
                nxt <- "first"

              }

              div(
                id = paste0("carousel-image", i),
                class = "carousel-image",
                image_constructor(imgs[[i]]),
                div(
                  class = "carousel-snapper",
                  a(
                    href = paste0("#carousel-image", prv_n),
                    class = "carousel-prev",
                    sprintf(goto, prv)
                  ),
                  a(
                    href = paste0("#carousel-image", nxt_n),
                    class = "carousel-next",
                    sprintf(goto, nxt)
                  )
                )
              )
            }
          )
        )
      )
    )

  } else {

    image_constructor(imgs[[1L]])

  }

}

index <- list()

# front page
front_page <- htmltools::withTags(
  htmltools::tagList(
    head(title("Flora of Finland"), meta_data, css, favicon),
    body(
      main(
        class = "main",
        div(
          class = "front-page-container",
          div(
            class = "front-page-text",
            h1(
              class = "front-page-title",
              "Flora of Finland"
            ),
            ul(
              class = "front-page-contents",
              li(a(href = "tracheophyta", "Vascular plants")),
              li(a(href = "taxa-index", "Index")),
              li(a(href = "glossary", "Glossary"))
            )
          ),
          div(
            class = "front-page-figure-container",
            figure(
              class = "figure",
              role = "group",
              img(
                class = c("main-img", "front-page-image"),
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
          )
        ),
        page_footer
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

  siblings <- setdiff(siblings, "favicon")

  prev_taxon <- siblings[which(siblings == taxon) - 1L]

  next_taxon <- siblings[which(siblings == taxon) + 1L]

  taxa <- list.dirs(page_dir, recursive = FALSE, full.names = FALSE)

  refs <- htmltools::withTags(
    lapply(
      content[["sources"]],
      \ (x) {
        mapply(
          \(ref, doi) {
            li(
              htmltools::HTML(ref),
              a(href = file.path("https://doi.org", doi), "doi:", doi)
            )
          },
          x[["ref"]],
          x[["doi"]],
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )
      }
    )
  )

  taxon_page <- htmltools::withTags(
    htmltools::tagList(
      head(title(name), meta_data, css, favicon),
      body(
        main(
          class = "main",
          nav_bar,
          div(
            class = "row1",
            # image
            div(
              class = "col2",
              if (!is.null(content[["images"]])) images(content)
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
              },
              # taxon name
              div(
                class = "page-title-container",
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
              div(
                class = "description-container",
                h3(class = "description-title", "Description"),
                if (!is.null(content[["description"]])) {
                  p(
                    class = "description",
                    link2glossary(content[["description"]])
                  )
                }
              )
            ),
            div(
              class = "col1",
              figure(
                class = "figure",
                role = "group",
                img(
                  class = "map",
                  src = "map.svg",
                  alt = sprintf(
                    "Occurrence map %s (%s) in Finland",
                    content[["vernacularName"]],
                    content[["scientificName"]]
                  )
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
          div(
            class = "row3",
            h3(class = "refs-title", "References:"), ol(class = "refs", refs)
          ),
          page_footer
        )
      )
    )
  )

  dir.create(
    file.path("build", taxon), showWarnings = FALSE, recursive = TRUE
  )

  htmltools::save_html(taxon_page, file.path("build", taxon, "index.html"))

  for (img in list.files(file.path("src", taxon), pattern = "\\.jpeg$")) {

    file.copy(
      file.path("src", taxon, img),
      file.path("build", taxon, img),
      overwrite = TRUE
    )

  }

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
    head(title("Index"), meta_data, css, favicon),
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

# glossary
glossary <- yaml::yaml.load_file("src/glossary.yml")

glossary <- glossary[order(vapply(glossary, getElement, "", "term"))]

glossary_page <- htmltools::withTags(
  htmltools::tagList(
    head(title("Glossary"), meta_data, css, favicon),
    body(
      main(
        class = "main",
        nav_bar,
        div(
          class = "glossary-container",
          div(
            class = "glossary-title-container",
            h1(class = "page-title", "Glossary")
          ),
          dl(
            class = "glossary",
            lapply(
              glossary,
              \(x) {
                list(
                  dt(id = gsub(" ", "-", tolower(x[["term"]])), x[["term"]]),
                  dd(link2glossary(x[["definition"]], ""))
                )
              }
            )
          )
        ),
        page_footer
      )
    )
  )
)

dir.create(
  file.path("build", "glossary"), showWarnings = FALSE, recursive = TRUE
)

htmltools::save_html(glossary_page, file.path("build", "glossary/index.html"))
