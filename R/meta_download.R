#' Downloads a PDF given a URL
#'
#' Download the PDF from the URL returned from Unpaywall or CrossRef.
#'
#' @param url A PDF URL from Unpaywall or CrossRef, as a string.
#' @param name The name to be given to the file.
#' @return Saves the PDF in the specified directory with the specified name.
#' @export
meta_download <- function(url, name) {

  filename <- file.path("PDFs", paste0(name, ".pdf"))

  tryCatch(
    download.file(url, filename),
    error = function(e) e
  )

}