#' Extract PDF links from CrossRef
#'
#' Get the link to the PDF for an article, given its DOI.
#'
#' @param doi The DOI of an article, as a string.
#' @return The URL to the PDF of an article, as a string.
#' @export
meta_crossref <- function(doi) {

  # If no DOI, then return NA
  if (doi ==  "NULL" | is.na(doi)) {
    return(NA)
  }


  # Get link to PDF
  pdfs <- tryCatch(crminer::crm_links(doi), error = function(e) e)


  # Return NA if it inherits an error
  if (inherits(pdfs, "error") | length(pdfs) == 0) {
    return(NA)
  }

  if ("pdf" %in% names(pdfs)) {
    return(as.character(pdfs$pdf))
  }

  if ("unspecified" %in% names(pdfs)) {
    return(as.character(pdfs$unspecified))
  }

  return(NA)
}