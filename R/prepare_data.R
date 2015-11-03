
## ---- read-ris-csv ----
read_ris_csv <- function (filename) {
    # define fields of interest: we'll only keep these columns 

    keep_cols <- c("A2", "AU", "JO", "KW", "N1", "T1", "T2", "TY", "Y1", "PB")
    ctypes <- setNames(rep(list("c"), length(keep_cols)), keep_cols)
    frm <- withCallingHandlers( 
        read_delim(filename, delim=",", quote="\"", escape_double=T,
            col_names=T, 
            col_types=do.call(cols_only, ctypes)
        ),
        warning=function (w) {
            if (str_detect(w$message,
                "^The following named parsers don't match the column names")) {
                invokeRestart("muffleWarning")
            }
        }
    )

    # a couple of files have some missing columns because these fields
    # were lacking in that run of entries,
    # producing a column mismatch when we try to rbind everything.
    # let's fix that by adding dummy values in those columns

    missing_cols <- setdiff(keep_cols, colnames(frm))
    if (length(missing_cols) > 0) {
        frm[ , missing_cols] <- ""
    }
    frm
}


## ---- field-parsing ----
# extract the finer-grained metadata from the note field N.B. There is
# sometimes a missing period at the end of a field, e.g. in acc nos.
# 1998070296, 1979107484, 1979202534, so this is imperfect



#' # Extract peer reviewing flag
#' # N.B. This information is very incomplete in the MLAIB
#' b %>% 
    mutate(peer=N1_field(N1, "Peer Reviewed") == "Yes")

#' # Extract publication type
#' # This is not just a recoding of the RIS field TY. Cross-tabulate.
#' b %>%
    mutate(pubtype=N1_field(N1, "Publication Type")) %>%
    count(TY, pubtype)



N1_field <- function (nn, field) {
    if (field == "DOI") {
        stop("Use N1_doi to extract DOI information.")
    }

    nn %>%
        str_extract(str_c("\\b", field, ": [^.]*\\.")) %>%
        str_replace(str_c("^", field, ": "), "") %>%
        str_replace("\\.$", "")
}

# DOI requires special treatment because it contains a period.
N1_doi <- . %>%
    str_extract("\\bDOI: [0-9a-zA-Z./]+\\.( |$)") %>%
    str_replace("^DOI: ", "") %>%
    str_replace("\\. ?$", "")

# never mind the song and dance of extracting month and day fields
Y1_Date <- . %>%
    str_sub(1, 4) %>%
    parse_date_time("%Y")



## ---- subject-parsing ----
strip_subject_relation <- function (s, pat) {
    s %>%
        str_replace_all(pat, "") %>%
        # also strip subheading
        str_replace(":.*$", "")
}

# table of ids and subject terms, one row for each id-term
# (removing relational terms) so that "Joyce, James" and
# "compared to Joyce, James" are collapsed into one hit
#
# the resulting data frame can be joined with the bib entries
subjects_frame <- function (bib) {
    rel_pat <- c("about", "after", "and", "application of( theories of)?",
        "as", "at", "between", "by", "compared to", "contributions of",
        "discusses", "during",
        "especially", "for", "from", "in", "includes",
        "influence( of| on)", "of", "on", "relationship to( the)?",
        "role( in| of)", "sources in", "study example",
        "the", "theories of",
        "theory of theories of", # Ortega
        "to( and from)?",
        "treatment( in| of( the| decadence)?)?", # decadence: Swinburne
        "use( in| of)", "with") %>%
    str_c(collapse="|") %>%
    str_c("^(", ., ") ") %>%
    # compile
    regex()

    bib %>%
        select(id, KW) %>%
        group_by(id) %>%
        transmute(term=str_split(KW, coll(";;"))) %>%
        do(data_frame(
            term=unlist(.$term)
        )) %>%
        ungroup() %>%
        # Strip relation terms and subheading, twice. This is a childish way
        # to deal with the grammar of subject relations, which can nest to
        # arbitrary depth; but in practice two passes suffices.  
        mutate(term=strip_subject_relation(term, rel_pat)) %>%
        mutate(term=strip_subject_relation(term, rel_pat)) %>%
        distinct(id, term)
}

subject_authors_frame <- . %>%

    # find authors by looking for subjects that have a (YYYY- in them
    # not perfect, because some books have a date range, like the Recherche,
    # so we'll need a list of non-authors (explore$non_authors) below. 
    filter(str_detect(term, " \\(\\d\\d\\d\\d\\??-")) %>%

    # MLAIB uses two U+00A0 NO-BREAK SPACEs in place of the death date
    # for living authors
    mutate(term=str_replace(term, "\\([-0-9? \\x{a0}/ca.]*\\)", "")) %>%
    mutate(term=str_trim(term)) %>%
    filter(!str_detect(term, "^The "))

subject_authors_last <- . %>%
    str_replace(",.*$", "") %>%
    stri_trans_tolower()


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





