#' Download file from PubMed Central
#'
#' Download the file of the designated PMCID (PubMed Central ID) in the file
#'     type of interest. This uses the
#'     [OAI-PMH service](https://www.ncbi.nlm.nih.gov/pmc/tools/oai/).
#'     The Open Access Index only maintains the full text for a subset of the
#'     articles on PMC (not sure which subset).
#'
#' @param pmcid The PMCID (not PMID) of the article as a string or integer
#'     (e.g. 13900).
#' @param file_name The file name, as a string (e.g. "my_folder/my_file.xml").
#'     If NULL (default), it will save the file in the current directory under
#'     its PMCID (e.g. "PMC 13900").
#' @param file_format Format as described on the OAI-PMH service. The default
#'     ("pmc") downloads the whole text in XML format.
#' @return Save the PubMed result as an XML file.
#' @export
mt_read_pmcoa <- function(pmcid, file_name = NULL, file_format = "pmc") {

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

    stop(paste("Error in", pmcid, "-", pmc_error))

  }

  # Full xpath under d1 namespace: d1:GetRecord/d1:record/d1:metadata
  # xml_ns(x) to see the namespaces; only the d1 namespace works
  pmc_record %>%
    xml2::xml_find_all(xpath = "//d1:metadata") %>%
    xml2::xml_contents() %>%
    xml2::xml_ns_strip()

  if (is.null(file_name)) {
    file_name = paste0("PMC ", pmcid, ".xml")
  }

  # Save
  invisible(xml2::write_xml(pmc_record, file_name))
}
