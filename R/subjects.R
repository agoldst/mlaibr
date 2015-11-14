

#' Remove relational phrases from subject headings
#'
#' Many subject headings specify a relation to a term: not just "Joyce, James"
#' but "compared to Joyce, James." For aggregating purposes, these relations can
#' often be ignored. This function attempts to remove these relational phrases
#' and leave only the main terms. Subheadings (delimited by a colon) are also
#' removed.
#'
#' Since the relation terms can be composed ("compared to treatment of"), the
#' function repeatedly strips the terms, up to a maximum number of iterations
#' given by the package option \code{mlaib.rel_strip_iterations}. It issues a
#' warning if relation terms are still present at the end.
#'
#' @param x character vector of headings
#'
#' @param rels character vector of regular expressions matching relation terms.
#'   The anchor \code{^} is added at the start and then the vector is collapsed
#'   using the alternator \code{|} as a separator. A default list is set as
#'   package option \code{mlaib.relations}, but this was found by trial and
#'   error and may need modification.
#'
#' @return the headings with relation terms removed
#'
#' @export
#'
strip_subject_relation <- function (x,
                                    rels=getOption("mlaib.relations")) {
    pat <- stringr::str_c(rels, collapse="|")
    pat <- stringr::str_c("^(", pat, ") ")
    pat <- stringr::regex(pat)
    result <- x
    iters <- getOption("mlaib.rel_strip_iterations")
    if (iters < 1) {
        stop("mlaib.rel_strip_iterations must be greater than 0")
    }
    for (i in 1:iters) {
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
#' It's often convenient to have a data frame with just subject terms and id's
#' that can be filtered and then joined back onto a table of bibliographic
#' items. This function is just a wrapper for filtering only \code{KW} fields
#' and then applying \code{\link{strip_subject_relation}}.
#'
#' @param bib long-format table of RIS information
#' @param rels character vector of relation patterns to pass to
#'   \code{\link{strip_subject_relation}}
#'
#' @return data frame with \code{id,value} columns
#'
#' @seealso \code{\link{subject_authors_frame}}
#'
#' @export
#'
subjects_frame <- function (bib, rels=getOption("mlaib.relations")) {
    result <- dplyr::filter_(bib, ~ field == "KW")
    result$value <- strip_subject_relation(result$value, rels)
    result <- dplyr::select_(result, ~ id, ~ value)
    result <- dplyr::distinct_(result, ~ id, ~ value)
    result
}

#' Filter a subjects frame down to author-subjects
#'
#' Given the result form \code{\link{subjects_frame}}, remove all but the names
#' of authors. Any birth-death date ranges after names are removed. No further
#' de-duplication is performed.
#'
#' Note that removing date ranges potentially also removes a disambiguation
#' among authors with the same name. On your head be it.
#'
#' @param x data frame with author terms in a \code{value} column
#'
#' @param non_authors (optional) character vector of names to reject that
#'   otherwise pass the filter.
#'
#' @return data frame like \code{x} but only rows where \code{value} is an
#'   author name retained. You might wish to use \code{\link[dplyr]{distinct}}
#'   to deduplicate this.
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







