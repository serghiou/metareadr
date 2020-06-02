#' A random sample from PubMed
#'
#' Identify a random sample of articles from PubMed. Note that this function
#'     retrieves the currently available articles from PubMed, so it requires
#'     internet connection to search PubMed and download the results. Depending
#'     on the connection and selected years, this can take at least a few
#'     minutes to run. Note that these samples will change with changes in the
#'     data provided by PubMed, for which reason a "Data sampled" column is
#'     added as a time-stamp. Please also consider using a seed to improve
#'     reproducibility and do not run this function repeatedly.
#'
#' @param n The number of articles to sample, as a numeric.
#' @param year_min The minimum year to sample from inclusive, as a numeric. By
#'     PubMed definition, this is the year of record creation.
#' @param year_max The maximum year to sample from inclusive, as a numeric. By
#'     PubMed definition, this is the year of record creation.
#' @param seed Provide a seed for a reproducible sample, as a numeric.
#' @return A tibble of randomly sampled articles and their PMID and Date sampled
#'     in yyyy-mm-dd.
#' @export
mt_sample_pubmed <- function(n = 1000,
                            year_min = 2020,
                            year_max = 2020,
                            seed = NULL) {

  # Build search term
  term <-
    sprintf('"%s" [Date - Create]: "%s" [Date - Create]', year_min, year_max)

  # Identify the PMID of all articles within the requested time period
  result <-
    rentrez::entrez_search(
      db = "pubmed",
      term = term,
      retmax = 222222222,
      use_history = T
    )

  # Set seed
  set.seed(seed)

  # Sample
  result %>%
    magrittr::use_series(ids) %>%
    tibble::tibble(pmid = .) %>%
    dplyr::sample_n(n) %>%
    dplyr::mutate(`Date sampled` = as.Date(Sys.time()))
}