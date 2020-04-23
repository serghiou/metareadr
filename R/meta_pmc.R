#' Download file from PubMed Central
#'
#' Download the file of the designated PMCID in the file type of interest. This
#'     uses the [OAI-PMH service](https://www.ncbi.nlm.nih.gov/pmc/tools/oai/).
#'     The Open Access Index only maintains the full text for a subset of the
#'     articles on PMC (not sure which subset).
#'
#' @param pmcid The PMCID of the article as a string or integer (e.g. 13900).
#' @param file The file name, as a string, without the file type (e.g.
#'     "my_file.xml").
#' @param file_format Format as described on the OAI-PMH service. The default
#'     ("pmc") downloads the whole text in XML format.
#' @return Save the PubMed result as an XML file.
#' @export
meta_pmcoa <- function(pmcid, file, file_format = "pmc") {

  base <- "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord"
  identifier <- "&identifier=oai:pubmedcentral.nih.gov:"
  xml_format <- "&metadataPrefix="
  uri <- paste0(base, identifier, pmcid, xml_format, file_format)

  pmc_record <- xml2::read_xml(uri)

  pmc_error <-
    pmc_record %>%
    xml2::xml_find_all(xpath = "d1:error") %>%
    xml2::xml_contents() %>%
    xml2::xml_text()

  if (!!length(pmc_error)) {

    return(paste("Error in", pmcid, "-", pmc_error))

  }

  # Full xpath under d1 namespace: d1:GetRecord/d1:record/d1:metadata
  # xml_ns(x) to see the namespaces; only the d1 namespace works
  pmc_record %>%
    xml2::xml_find_all(xpath = "//d1:metadata") %>%
    xml2::xml_contents() %>%
    xml2::write_xml(paste0(file, ".xml"))

}
