#' Extract PDFs from SciHub
#'
#' Extract PDFs from SciHub. Note that this may be illegal in your country or
#'     institution. Note, also, that SciHub is very strict with automated
#'     downloads and using a VPN to cycle between IPs may be necessary.
#'
#' @param x Either a DOI or PMID as a string.
#' @param filename The name of the file to be saved, as a string.
#' @param filedir The name of the directory in which to save the file.
#' @return Saves the PDF in the specified directory with the specified name.
#' @export
meta_scihub <- function(x, filename = NULL, filedir = "PDF_scihub") {

  if (is.null(filename)) {

    filename = x

  }

  base_url_scihub <- "https://sci-hub.se"
  base_url_pubmed <- "https://www.ncbi.nlm.nih.gov/pubmed"
  file_name <- file.path(filedir, paste0(filename, ".pdf"))

  is_pmid <- grepl("^[0-9]+$", x)

  if (is_pmid) {

    sci_url <- file.path(base_url_scihub, base_url_pubmed, x)

  } else {

    sci_url <- file.path(base_url_scihub, x)

  }


  sci_html <-
    tryCatch(
      httr::GET(sci_url, timeout(5)) %>% xml2::read_html(),
      error = function(e) e
    )

  if (inherits(sci_html, "error")) {

    return(message(paste("PMID", x, "was not found - 1.")))

  }


  pdf_url <-
    sci_html %>%
    xml2::xml_find_all(xpath = '//*[@id="buttons"]/ul/li/a') %>%
    xml2::xml_attrs() %>%
    unlist() %>%
    magrittr::extract("onclick") %>%
    gsub("^.*//(.*=true).*$", "\\1", .)
  # str_extract("sci-.*true") # not general enough e.g. //dacemirror.sci...


  tryCatch(
    download.file(pdf_url, destfile = file_name),
    error = function(e) message(paste("PMID", x, "was not found - 2."))
  )

  Sys.sleep(3)
}