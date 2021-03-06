% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mt_unpaywall.R
\name{mt_unpaywall}
\alias{mt_unpaywall}
\title{Extract all information held about an article on Unpaywall}
\usage{
mt_unpaywall(doi)
}
\arguments{
\item{doi}{A DOI as a string or a vector of DOI strings.}
}
\value{
A tibble of all articles of interest and their fields.
}
\description{
Extract all information held about an article on Unpaywall. Designed to not
    exceed the appropriate API limit.
}
