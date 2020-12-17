#' Extract all information held about an article on Unpaywall
#'
#' Extract all information held about an article on Unpaywall. Designed to not
#'     exceed the appropriate API limit.
#'
#' @param doi A DOI as a string or a vector of DOI strings.
#' @return A tibble of all articles of interest and their fields.
#' @export
mt_unpaywall <- function(doi) {

  # TODO Make sure it does not exceed the API limit!

  unpaywall_list <-
    doi %>%
    magrittr::set_names(., .) %>%
    pbapply::pblapply(.get_unpaywall)

  unpaywall_list[!is.na(unpaywall_list)] %>%
    dplyr::bind_rows() %>%
    tibble::rowid_to_column()

}


#' Extract all information held about an article on Unpaywall.
#'
#' @param doi A single DOI as a string.
#' @param email Your email address, as a string.
#' @return A tibble of all articles of interest and their fields.
.get_unpaywall <- function(doi, email = "sstelios@gmail.com") {

  # If no DOI, then return NA
  if (doi ==  "NULL" | is.na(doi)) {
    return(NA)
  }

  # Get link to PDF
  unpaywall_data <-
    tryCatch(
      roadoi::oadoi_fetch(doi, email = email),  # .progress = "text"
      error = function(e) e
    )

  # Return NA if it inherits an error
  if (inherits(unpaywall_data, "error") | length(unpaywall_data) == 0) {

    return(NA)

  } else {

    return(unpaywall_data)

  }
}