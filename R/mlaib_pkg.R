


.onLoad <- function(libname, pkgname) {
    op_old <- options()
    op <- list(
        mlaib.ris_keep=c(
            "A2", "AU", "JO", "KW", "N1",
            "T1", "T2", "TY", "Y1", "PB"
        ),
        mlaib.relations=c(
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
        mlaib.rel_strip_iterations=2
    )
  to_set <- !(names(op) %in% names(op_old))
  if (any(to_set)) options(op[to_set])

  invisible()
}


