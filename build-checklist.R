req <- httr2::request("https://api.laji.fi")
req <- httr2::req_url_path(req, "v0")
req <- httr2::req_url_path_append(req, "taxa")
req <- httr2::req_url_query(
  req,
  pageSize = 1000,
  includeHidden = "true",
  onlyFinnish = "true",
  selectedFields = paste(
    "id",
    "taxonConceptIds",
    sep = ","
  ),
  access_token = Sys.getenv("FINBIF_ACCESS_TOKEN"),
  page = 1
)

res <- httr2::req_perform(req)
res <- httr2::resp_body_json(res)

ans <- res[["results"]]

for (i in seq_len(res[["lastPage"]])[-1]) {

  req <- httr2::req_url_query(req, page = i)

  res <- httr2::req_perform(req)
  res <- httr2::resp_body_json(res)

  ans <- c(ans, res[["results"]])

}

tmp <- tempfile()

download.file(
  file.path(
    "https://cdn.laji.fi",
    "files",
    "checklists",
    "2023",
    "Liite1_Appendix1_Lajiluettelo2023_Checklist2023.xlsx"
  ),
  destfile = tmp,
  quiet = TRUE
)

ids <- data.frame(Identifier = vapply(ans, getElement, "", "id"))

checklist <- readxl::read_xlsx(tmp, col_types = "text", progress = FALSE)

checklist <- merge(checklist, ids, all.x = TRUE)

names(checklist) <- c(
  "taxonID", "domain", "kingdom", "phylum", "subphylum", "division",
  "class", "subclass", "order", "suborder", "superfamily", "family",
  "subfamily", "tribe", "subtribe", "genus", "subgenus", "aggregate",
  "taxonRank", "scientificName", "scientificNameAuthorship", "vernacularName",
  "swedishName", "altVernacularNames", "experts", "informalGroups"
)

checklist <- subset(
  checklist,
  order == "Polypodiales" & taxonRank == "species",
  c(taxonID, phylum, class, order, family, genus, scientificName)
)

write.table(
  checklist,
  "checklist.csv",
  quote = FALSE,
  sep = ",",
  na = "",
  row.names = FALSE
)
