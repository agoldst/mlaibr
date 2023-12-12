
#' Extract finer-grained metadata from the note field
#'
#' In the MLA bibliography, the N1 (note) field contains further metadata which
#' is general consistently identified and can be programmatically extracted.
#' Examples include the \code{Accession Number}, \code{Publication Type},
#' \code{DOI}, and \code{Peer Reviewed} fields.
#'
#' Fields are reliably delimited only by a terminating period, and there is
#' sometimes a missing period at the end of a field, e.g. in MLAIB acc nos.
#' 1998070296, 1979107484, 1979202534. If the field data contains a period this
#' is a problem, but the only case in which I know this to happen is in
#' \code{DOI} fields. This is treated as a special case.
#'
#' @param x character vector
#' @param field name of field to extract (without colon). Case sensitive.
#'
#' @return character vector of extracted values. Validate carefully. Missing
#'   values are represented as \code{NA}.
#'
#' @examples
#' \dontrun{
#' b <- read_ris("bib.ris") |>
#'     spread_ris()
#'
#' # Extract peer reviewing flag
#' # N.B. This information is very incomplete in the MLAIB
#' b |>
#'     mutate(peer=N1_field(N1, "Peer Reviewed") == "Yes")
#'
#' # Extract publication type
#' # This is not just a recoding of the RIS field TY. Cross-tabulate.
#' b |>
#'     mutate(pubtype=N1_field(N1, "Publication Type")) |>
#'     count(TY, pubtype)
#' }
#'
#' @export
#'
N1_field <- function (x, field) {
    if (field == "DOI") {
        return(N1_doi(x))
    }

    result <- stringr::str_extract(x,
        stringr::str_c("\\b", field, ": [^.]*\\."))
    result <- stringr::str_replace(result,
        stringr::str_c("^", field, ": "), "")
    stringr::str_replace(result, "\\.$", "")
}

# DOI requires special treatment because it contains a period.
N1_doi <- function (x) {
    result <- stringr::str_extract(x, "\\bDOI: [0-9a-zA-Z./]+\\.( |$)")
    result <- stringr::str_replace(result, "^DOI: ", "")
    stringr::str_replace(result, "\\. ?$", "")
}

#' Naive parsing of dates into years
#'
#' The \code{Y1} field holds the publication date information. This normally
#' contains a year, and sometimes also contains month, day, or season
#' information. This function extracts just the year and recodes it as a
#' \code{Date} on the first day of the year.
#'
#' @param x character vector (Y1 field)
#' @return vector of \code{Date}s corresponding to years
#'
#' @examples
#' \dontrun{
#' # To get at the messy finer-grained information, try:
#' str_split_fixed(x, coll("/"), 4)
#' }
#'
#' @export
#'
Y1_year <- function (x) {
    y <- stringr::str_sub(x, 1, 4)
    as.Date(stringr::str_c(y, "-01-01"))
}
