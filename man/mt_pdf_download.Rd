% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mt_pdf_download.R
\name{mt_pdf_download}
\alias{mt_pdf_download}
\title{Downloads a PDF given a URL}
\usage{
mt_pdf_download(url, name)
}
\arguments{
\item{url}{A PDF URL from Unpaywall or CrossRef, as a string.}

\item{name}{The name to be given to the file.}
}
\value{
Saves the PDF in the specified directory with the specified name.
}
\description{
Download the PDF from the URL returned from Unpaywall or CrossRef.
}
