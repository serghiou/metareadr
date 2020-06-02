#' Find an issue-match to a PubMed article
#'
#' Identify the PMID of an article in the same issue as the provided article.
#'
#' @param pmid The PMID of the article to which matches are desired.
#' @param n_max The maximum number of records to download from the issue.
#' @return A table of articles from the same journal issue as the provided PMID.
#' @export
mt_match <- function(pmid = 16060722, n_max = 100) {

  # Get citation of case
  matches_to <- pmid
  term <- paste(matches_to, "[uid]")
  case <- mt_read_pubmed(term)

  journal <- case %>% xml2::xml_find_all("//Journal/Title") %>% xml2::xml_text()
  volume <- case %>% xml2::xml_find_all("//Volume") %>% xml2::xml_text()
  issue  <- case %>% xml2::xml_find_all("//Issue") %>% xml2::xml_text()

  # Build search terms for controls
  term <- sprintf('"%s" [Journal]', journal)

  if (!!length(volume)) {
    term <- paste(term, sprintf("%s [Volume]", volume), sep = " AND ")
  }

  if (!!length(issue)) {
    term <- paste(term, sprintf("%s [Issue]", issue), sep = " AND ")
  }

  # Get controls
  term %>%
    mt_read_pubmed(n_max = n_max) %>%
    mt_extract_pubmed() %>%
    dplyr::mutate(matches_to = matches_to, .before = 1)
}
