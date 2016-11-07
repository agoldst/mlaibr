
#' Explore MLA bibliography data
#'
#' This package gives some rudimentary utilities for loading and preprocessing
#' MLA International Bibliography data or other RIS-formatted bibliographic
#' data.
#'
#' @name mlaibr
#' @docType package
#'
NULL


#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# initialize package options if they haven't been set
.onLoad <- function(libname, pkgname) {
    op_old <- options()
    op <- list(
        mlaibr.ris_keep=c(
            "A2", "AU", "JO", "KW", "N1",
            "T1", "T2", "TY", "Y1", "PB"
        ),
        mlaibr.relations=c(
            "about", "after", "and", "application of( theories of)?",
            "as", "at", "between", "by", "compared to", "contributions of",
            "discusses", "during",
            "especially", "for", "from", "in", "includes",
            "influence( of| on)", "of", "on", "relationship to( the)?",
            "role( in| of)", "sources in", "study example",
            "the", "theories of",
            "to( and from)?",
            "treatment( in| of( the)?)?",
            "use( in| of)", "with"
        )
    )
  to_set <- !(names(op) %in% names(op_old))
  if (any(to_set)) options(op[to_set])

  invisible()
}


