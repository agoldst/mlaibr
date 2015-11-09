#' Extract finer-grained metadata from the note field
#'
#' In the MLA bibliography, the N1 (note) field contains further metadata which is general consistently identified and can be programmatically extracted. Examples include the \code{Accession Number}, \code{Publication Type}, \code{DOI}, and \code{Peer Reviewed} fields.
#'
#' Fields are reliably delimited only by a terminating period, and 
#' there is
#' sometimes a missing period at the end of a field, e.g. in MLAIB acc nos.
#' 1998070296, 1979107484, 1979202534. If the field data contains a period this 
#' is a problem, but the only case in which I know this to happen is in \code{DOI} fields. This is treated as a special case.
#'
#' @param x character vector
#' @param field name of field to extract (without colon). Case sensitive.
#'
#' @return character vector of extracted values. Validate carefully. Missing values are represented as \code{NA}.
#'
#' @examples
#' \dontrun{
#' b <- read_ris("bib.ris") %>%
#'     spread_ris()
#'
#' # Extract peer reviewing flag
#' # N.B. This information is very incomplete in the MLAIB
#' b %>% 
#'     mutate(peer=N1_field(N1, "Peer Reviewed") == "Yes")
#'
#' # Extract publication type
#' # This is not just a recoding of the RIS field TY. Cross-tabulate.
#' b %>%
#'     mutate(pubtype=N1_field(N1, "Publication Type")) %>%
#'     count(TY, pubtype)
#' }
#'
#' @export
#'
N1_field <- function (x, field) {
    if (field == "DOI") {
        return(N1_doi(x))
    }

    result <- stringr::str_extract(nn,
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
#' The \code{Y1} field holds the publication date information. This normally contains a year, and sometimes also contains month, day, or season information. This function extracts just the year and recodes it as a \code{Date} on the first day of the year.
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
    as.Date(stringr::str_c(y, "-01-01")
}


#' Remove relational phrases from subject headings
#'
#' Many subject headings specify a relation to a term: not just "Joyce, James" but "compared to Joyce, James." For aggregating purposes, these relations can often be ignored. This function attempts to remove these relational phrases and leave only the main terms. Subheadings (delimited by a colon) are also removed.
#'
#' Since the relation terms can be composed ("compared to treatment of"), the function repeatedly strips the terms, up to a maximum number of iterations given by the package option \code{mlaib.rel_strip_iterations}. It issues a warning if relation terms are still present at the end.
#'
#' @param x character vector of headings
#'
#' @param rels character vector of regular expressions matching relation terms. The anchor \code{^} is added at the start and then the vector is collapsed using the alternator \code{|} as a separator. A default list is set as package option \code{mlaib.relations}, but this was found by trial and error and may need modification.
#'
#' @return the headings with relation terms removed
#'
#' @export
#'
strip_subject_relation <- function (x,
                                    rels=options("mlaib.relations")) {
    stopifnot(times > 0)
    pat <- stringr::str_c(rels, collapse="|")
    pat <- stringr::str_c("^(", pat, ") ")
    pat <- stringr::regex(pat)
    result <- x
    for (i in 1:options("mlaib.rel_strip_iterations")) {
        result <- stringr::str_replace_all(result, pat, "")
        rels_still <- any(stringr::str_detect(result, pat))
        if (!rels_still) {
            break
        }
    }
    if (rels_still) {
        warning("Some relation terms still remain after ", n, " iterations.")
    }
    # also strip any subheading
    result <- stringr::str_replace(result, ":.*$", "")
    result
}

#' Generate a data frame of the id-subject-heading table
#'
#' It's often convenient to have a data frame with just subject terms and id's that can be filtered and then joined back onto a table of bibliographic items. This function is just a wrapper for filtering only \code{KW} fields and then applying \code{\link{strip_subject_relation}}.
#'
#' @param bib long-format table of RIS information
#' @param rels character vector of relation patterns to pass to \code{\link{strip_subject_relation}}
#'
#' @return data frame with \code{id,value} columns
#'
#' @seealso \code{\link{subject_authors_frame}}
#'
#' @export
#'
subjects_frame <- function (bib, rels=options("mlaib.relations")) {
    result <- dplyr::filter_(bib, ~ field == "KW")
    result$value <- strip_subject_relation(result$value, rels)
    result <- dplyr::select_(~ id, ~ value)
    result <- dplyr::distinct_(~ id, ~ value)
    result
}

#' Filter a subjects frame down to author-subjects
#'
#' Given the result form \code{\link{subjects_frame}}, remove all but the names 
#' of authors. Any birth-death date ranges after names are removed. No further 
#' de-duplication is performed.
#'
#' Note that removing date ranges potentially also removes a disambiguation 
#' among authors were the same name. On your head be it.
#'
#' @param x data frame with author terms in a \code{value} column
#'
#' @param non_authors (optional) character vector of names to reject that 
#' otherwise pass the filter.
#'
#' @return data frame like \code{x} but only rows where \code{value} is an author name 
#' retained. You might wish to 
#' use \code{\link[dplyr]{distinct}} to deduplicate this.
#'
#' @seealso \code{\link{subjects_frame}}
#'
#' @export
#' 
subject_authors_frame <- function (x, non_authors=NULL) {

    # find authors by looking for subjects that have a (YYYY- in them
    x <- dplyr::filter_(x,
        ~ stringr::str_detect(value, " \\(\\d\\d\\d\\d\\??-"))
    # MLAIB uses two U+00A0 NO-BREAK SPACEs in place of the death date
    # for living authors
    x$value <- stringr::str_trim(
        stringr::str_replace(x$value, "\\([-0-9? \\x{a0}/ca.]*\\)", "")
    )

    # not perfect, because some books have a date range, like the Recherche.
    # Assuming no one is named The X, this gets rid of some more
    x <- dplyr::filter_(x, ~ !stringr::str_detect(value, "^The "))

    # and an explicit list, if present, can filter further
    if (length(non_authors) > 0) {
        flt <- lazyeval::interp(~ !(value %in% xs), xs=non_authors)
        x <- dplyr::filter_(x, flt)
    }

    x
}

#' Author term to lowercase last name only
#'
#' A convenience function for stripping down to an author keyword.
#'
#' @param values character vector of \code{"Last, First ..."} terms
#'
#' @return character vector of \code{"last"} names
#'
#' @export
#'
subject_authors_last <- function (values) {
    result <- stringr::str_replace(values, ",.*$", "")
    stringi::stri_trans_tolower(result)
}


# derive author series from subject series
subject_author_series <- function(series, non_authors) {
    auth_norm <- subject_author_norm(series, non_authors)

    series %>% ungroup() %>%
        inner_join(auth_norm, by="term") %>%
        select(-term) %>%
        group_by(date, author) %>%
        summarize(weight=sum(weight)) %>%
    # new baseline for freq: authors compared to other authors within
    # a date interval
        mutate(freq=weight / sum(weight))
}


title_author_series <- function(term_year, authors) {
    authors <- sort(unique(authors))
    term_year <- term_year[rownames(term_year) %in% authors, ]
    message(nrow(term_year)," last names matched in titles")

    term_year %>%
        as.matrix() %>%
        as.data.frame() %>%
        mutate(term=rownames(term_year)) %>%
        gather(date, weight, -term) %>%
        group_by(date) %>%
        mutate(freq=weight / sum(weight)) 
}


subject_series <- function(bib, kws, interval) {
    bib$date <- cut.Date(bib$date, interval, ordered=T)
    bib %>% ungroup() %>%
        select_(~ id, ~ date, ~ weight) %>%
        # use kws for keywords
        inner_join(kws, by="id") %>%
        # now aggregate
        group_by(date, term) %>%
        summarize(weight=sum(weight),
                  doc_weight=n_distinct(id)) %>%
        mutate(freq=weight / sum(weight),
               doc_freq=doc_weight / sum(doc_weight))
}





