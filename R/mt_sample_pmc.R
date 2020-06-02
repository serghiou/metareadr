#' A random sample from PubMed Central (PMC)
#'
#' Identify a random sample of articles. Note that this function retrieves the
#'     list of currently available articles from PMC, so it requires internet
#'     connection to download a file of ~550 MB at the time of writing and,
#'     depending on the connection, it will take about a minute to run. Note
#'     that these samples will change with changes in the data provided by PMC,
#'     for which reason a "Data sampled" column is added as a time-stamp.
#'
#' @param n The number of articles to sample, as a numeric.
#' @param year_min The minimum year to sample from inclusive, as a numeric.
#' @param year_max The maximum year to sample from inclusive, as a numeric.
#' @param seed Provide a seed for a reproducible sample, as a numeric.
#' @return A tibble of randomly sampled articles and their PMCID, PMID, DOI,
#'     Journal Title, ISSN, eISSN, Year, Volume, Issue, Page and Date sampled.
#'     Date sampled is in yyyy-mm-dd.
#' @export
meta_sample_pmc <- function(n = 1000,
                            year_min = 2016,
                            year_max = 2020,
                            seed = NULL) {

  # Download file
  ftp <- "https://ftp.ncbi.nlm.nih.gov/pub/pmc/PMC-ids.csv.gz"
  pmc <- suppressWarnings(readr::read_csv(ftp))

  # Set seed
  set.seed(seed)

  # Sample
  pmc %>%
    dplyr::filter(Year >= year_min & Year <= year_max) %>%
    dplyr::sample_n(n) %>%
    dplyr::select(PMCID, PMID, DOI, `Journal Title`:Page, `Release Date`) %>%
    dplyr::mutate_at(vars(PMID, Volume:Page), as.character) %>%
    dplyr::mutate(`Date sampled` = as.Date(Sys.time()))
}