#' Extract fields of interest from PubMed results
#'
#' Extract all fields of interest for each article.
#'
#' @param pubmed_xml The PubMed XML results file, as a string.
#' @param cl The number of CPU cores as a number to be passed to pblapply.
#' @return A tibble of all articles of interest and their fields.
#' @export
meta_table <- function(pubmed_xml, cl = 7) {

  article_xml <- xml2::xml_find_all(pubmed_xml, "PubmedArticle")

  article_xml %>%
    pbapply::pblapply(.extract_fields, cl = cl) %>%
    dplyr::bind_rows() %>%
    tibble::rowid_to_column()
}


#' Extract fields of interest from a single PubMed article result file
#'
#' Extract all fields of interest for an article.
#'
#' @param xml_doc The PubMed XML result file for a single study, as a string.
#' @return A tibble of all articles of interest and their fields.
.extract_fields <- function(xml_doc) {

  xpath_list <- list()

  xpath_list[["pmid"]] <- "MedlineCitation/PMID"
  xpath_list[["doi"]]  <- "MedlineCitation/Article/ELocationID[@EIdType='doi']"

  field_list <- lapply(xpath_list, .extract_xml, xml_doc = xml_doc)
  field_list[lapply(field_list, length) == 0] <- NA

  tibble::as_tibble(field_list)
}


#' Extract text of interest from an XML file
#'
#' Extract text of interest from an XML file.
#'
#' @param xml_doc The XML document as it was returned from xml2.
#' @return A string of interest.
.extract_xml <- function(xml_doc, xpath) {

  xml_doc %>%
    xml2::xml_find_all(xpath = xpath) %>%
    xml2::xml_contents() %>%
    xml2::xml_text()

}