% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_ris.R
\name{read_ris_csv}
\alias{read_ris_csv}
\title{Read in a CSV file of converted RIS data}
\usage{
read_ris_csv(filename, columns = getOption("mlaibr.ris_keep"))
}
\arguments{
\item{filename}{file to read in, produced from \code{\link{convert_ris}}.}

\item{columns}{RIS fields to retain (NULL to retain all)}
}
\value{
wide format data frame with one row for each item and one column for
  each field.
}
\description{
Deprecated. Use \code{\link{read_ris}} unless the speed gain is important.
Read in the result of \code{\link{convert_ris}}. Note that this function
returns a \emph{wide}-format rather than long-format data frame and does not
add any ID column.
}

