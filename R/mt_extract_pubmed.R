#' Extract fields of interest from PubMed results
#'
#' Extract all fields of interest for each article.
#'
#' @param pubmed_xml The PubMed XML results file, as a string.
#' @param cl The number of CPU cores as a number to be passed to `pblapply`.
#' @return A tibble of all articles of interest and their fields.
#' @export
mt_extract_pubmed <- function(pubmed_xml, cl = 7) {

  # Move into the nodeset level of each PubmedArticle
  article_xml <- xml2::xml_find_all(pubmed_xml, "PubmedArticle")

  # Extract fields of interest
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

  # Define the XPath for the fields of interest
  xpath_list <- list(
    pmid = "MedlineCitation/PMID",
    doi = "PubmedData/ArticleIdList/ArticleId[@Idtype='doi']",
    pmcid = "PubmedData/ArticleIdList/ArticleId[@Idtype='pmc']",
    pii = "PubmedData/ArticleIdList/ArticleId[@Idtype='pii']",
    title = "MedlineCitation/Article/ArticleTitle",
    pub_date_year = "MedlineCitation/Article/Journal//PubDate/Year",
    pub_date_month = "MedlineCitation/Article/Journal//PubDate/Month",
    pub_date_day = "MedlineCitation/Article/Journal//PubDate/Day",
    journal = "MedlineCitation/Article/Journal",
    volume = "MedlineCitation/Article/Journal/JournalIssue/Volume",
    issue = "MedlineCitation/Article/Journal/JournalIssue/Issue",
    abstract = "MedlineCitation/Article/Abstract",
    publication_type = "MedlineCitation/Article/PublicationTypeList/PublicationType",
    mesh = "MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName",
    reference_pmids = "PubmedData/ReferenceList//ArticleId[@IdType='pubmed']"
  )

  # Extract fields of interest
  field_list <- lapply(xpath_list, .extract_xml, xml_doc = xml_doc)

  # Turn empty list levels into NA
  field_list[lapply(field_list, length) == 0] <- NA

  # Export as tibble
  tibble::as_tibble(field_list)
}


#' Extract text of interest from an XML file
#'
#' Extract text of interest from an XML file.
#'
#' @param xml_doc The XML document as it was returned from xml2.
#' @param xpath The XPath to the item of interest.
#' @return A string of interest.
.extract_xml <- function(xml_doc, xpath) {

  xml_doc %>%
    xml2::xml_find_all(xpath = xpath) %>%
    xml2::xml_contents() %>%
    xml2::xml_text() %>%
    paste(collapse = "; ")
}
