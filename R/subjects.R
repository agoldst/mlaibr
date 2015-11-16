

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
#' It's often useful to have a data frame with just subject terms and id's that
#' can be filtered and then joined back onto a table of bibliographic items.
#' This function is just a convenience wrapper for filtering only \code{KW}
#' fields and then applying \code{\link{strip_subject_relation}}.
#'
#' @param bib long-format table of RIS information with \code{id,field,value}
#'   columns from \code{\link{read_ris}}
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

#' Detect author names among subject headings
#'
#' Given a vector of subject terms, determine which are (likely) author terms.
#'
#' The heuristic is simply to look for a (possibly open-ended) birth-death date
#' range in the term. Note that this is highly imperfect, since books and
#' periodicals often also have date ranges. I have not found an alternative to
#' removing these by hand.
#'
#' @param x character vector of subject terms
#'
#' @return logical vector indicating whether the corresponding element of
#'   \code{x} is an author term.
#'
#' @seealso \code{\link{subjects_frame}}, \code{\link{subject_author}}
#'
#' @examples
#' is_author("Joyce, James (1882-1941)")
#'
#' is_author("South Asian literature")
#'
#' # but N.B.
#' is_author("A la recherche du temps perdu (1913-1929)")
#'
#' @export
#'
is_author <- function (x) {
    # find authors by looking for subjects that have a birthdate
    # indication (YYYY- or (YYY-) or (ca. YYYY-) etc. MLAIB uses
    # no-break spaces after ca. and fl. This is not perfect, because
    # some books have a date range, like the Recherche.
    result <- stringr::str_detect(x,
        " \\((fl\\.\\x{a0})?(ca\\.\\x{a0})?\\d\\d\\d\\d?\\??-")
    # Assuming no one is named The X, this gets rid of some more
    result <- result & !stringr::str_detect(x, "^The ")
    result
}

#' Convert subject terms for authors to names alone
#'
#' Given subject terms containing author names, strip away date ranges to yield
#' \code{"Last, First Middle Middle"} names. No further de-duplication is
#' performed. Subheadings and relation terms are assumed already stripped (as by
#' \code{\link{strip_subject_relation}}).
#'
#' Note that removing date ranges potentially also removes a disambiguation
#' among authors with the same name. On your head be it.
#'
#' @param x character vector of subject terms
#' @return character vector of author terms
#'
#' @seealso \code{\link{subjects_frame}}, \code{\link{is_author}}
#'
#' @examples
#' subject_author("Joyce, James (1882-1941)")
#'
#' @export
#'
subject_author <- function (x) {
    # date ranges can include numbers, dashes, spaces, no-break spaces,
    # and "ca." and "fl."
    stringr::str_trim(
        stringr::str_replace(x, "\\([-0-9? \\x{a0}/cafl.]*\\)", "")
    )
}

#' Author term to lowercase last name only
#'
#' A convenience function for stripping down to an author keyword.
#'
#' @param x character vector of \code{"Last, First ..."} terms
#'
#' @return character vector of \code{"last"} names
#'
#' @export
#'
subject_author_last <- function (x) {
    result <- stringr::str_replace(x, ",.*$", "")
    stringi::stri_trans_tolower(result)
}







