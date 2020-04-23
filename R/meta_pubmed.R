#' Download PubMed results as XML
#'
#' Saves the meta-data of a PubMed query as an XML file.
#'
#' @param term The search term, e.g. "meta-analysis[TITLE]", as a string.
#' @param filename The name of the XML file to save as a string.
#' @param n_max The maximum number of records to download, as an integer.
#' @return Save the PubMed result as an XML file.
#' @export
meta_pubmed <- function(term, filename = "pubmed_info.xml", n_max = 10000) {

  esearch <- rentrez::entrez_search(
    db = "pubmed",
    term = term,
    retmax = n_max,
    use_history = T
  )

  efetch <- rentrez::entrez_fetch(
    db = "pubmed",
    web_history = esearch$web_history,
    rettype = "xml"
  )

  write(efetch, file = filename)
}