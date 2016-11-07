#' Read RIS files into a long-format data frame
#'
#' This function loads one or more RIS files into a long data frame. It assigns
#' unique record ID numbers and optionally filters out unwanted fields. The
#' result has one line for each record-field combination, including possible
#' repeats when a record has multiple instances of a field. The result is not
#' quite "tidy," because no special parsing is done to extract information
#' within the catch-all \code{N1} field; use \code{\link{N1_field}} for that.
#' Note also that all fields remain in string format; for example, dates are not
#' parsed (see \code{\link{Y1_year}}). To project to a data frame with one row
#' for each bibliographic record, use \code{\link{spread_ris}}.
#'
#' @param filenames vector of names of RIS files to read. Can also be a list of
#' connections, or a single connection. As a convenience, if a name ends in
#' "\code{.zip}," the file will be assumed to be a zip archive containing a
#' single RIS file (which is the format supplied in EBSCOhost exports).
#' @param fields which RIS fields to keep in the result. A default list is set
#'   by the package option \code{mlaibr.ris_keep}. To keep all fields, set
#'   \code{fields=NULL}.
#'
#' @return a data frame with columns \code{id,field,value}
#'
#' @seealso \code{\link{spread_ris}}
#' @export
#'
read_ris <- function (filenames, fields=getOption("mlaibr.ris_keep"),
                      src_labels=NULL) {
    result <- vector("list", length(filenames))
    base_id <- 0

    # subscripting a single connection will de-class it, so handle this case
    # specially
    if (inherits(filenames, "connection")) {
        filenames <- list(filenames)
    }

    # construct filter if fields specified
    if (length(fields) > 0) {
        if (!is.null(src_labels)) {
            fields <- c(fields, "src")
        }
        flt <- lazyeval::interp(~ field %in% x, x=fields)
    } else {
        flt <- ~ TRUE
    }
    p <- dplyr::progress_estimated(length(filenames))
    for (i in seq_along(filenames)) {
        f <- filenames[[i]]

        close_con <- FALSE
        if (is.character(f) && grepl("\\.zip$", f)) {
            f <- unz(f, unzip(f, list=TRUE)[["Name"]])
            close_con <- TRUE
        }
        frm <- read_ris_file(f, src=src_labels[[i]])
        frm <- dplyr::filter_(frm, flt)

        # adjust ID number
        frm$id <- frm$id + base_id
        result[[i]] <- frm
        base_id <- base_id + tail(frm$id, 1)
        p$tick()
        if (close_con) close(f)
    }


    dplyr::bind_rows(result)
}

# Workhorse for reading a single RIS file into a data frame
read_ris_file <- function (f, src=NULL) {
    ll <- readLines(f, encoding="UTF-8")

    # locate end-of-record lines
    ers <- stringr::str_detect(ll, "^ER  -")

    # assign sequential record ids
    result <- data.frame(id=cumsum(ers) + 1, value=ll,
                         stringsAsFactors=FALSE)

    # drop ER lines
    result <- result[!ers, ]

    # drop blank lines
    result <- result[stringr::str_detect(result$value, "\\S"), ]

    # split field key from value
    sp <- stringr::str_split(result$value, stringr::coll("  -"), n=2)
    result$field <- vapply(sp, `[[`, "", 1)
    result$value <- stringr::str_trim(vapply(sp, `[[`, "", 2))

    # rename columns
    result <- result[ , c("id", "field", "value")]

    if (any(!stringr::str_detect(result$field, "^\\w\\w$"))) {
        warning(
"Invalid RIS fields found, probably because of a parsing problem."
        )
    }
    if (any(result$field == "ER")) {
        warning(
"Some ER (end-of-record) fields were not eliminated, probably because of a parsing problem."
        )
    }

    # Add a source label, if given, as a dummy field "src", one for each
    # record
    if (!is.null(src)) {
        ids <- seq(result[["id"]][nrow(result)])
        result <- dplyr::bind_rows(
            dplyr::data_frame_(list(
                id=~ ids, field= ~ "src", value= ~ src
            )),
            result
        )
        result <- dplyr::arrange_(result, ~ id)
    }

    dplyr::tbl_df(result)
}

#' Convert bibliographic records from long to wide format
#'
#' The data frame from \code{\link{read_ris}} has many rows for each
#' bibliographic record. Many questions relate to bibliographic items as a unit,
#' in which case it is convenient to have a data frame in the corresponding wide
#' form. However, note that this is a somewhat non-normalized format since many
#' RIS fields can repeat within a record. Repeat fields are joined into a single
#' string value.
#'
#' It would be a little more formal to produce list columns from repeating
#' fields, but that's just a drag. It may make more sense to normalize by hand
#' by making separate tables of the repeating columns and keying to ID (or
#' accession number), then spreading out only the single columns of interest
#' here.
#'
#' @param x data frame with \code{id,field,value} columns
#' @param multi_sep separator string for repeated fields. Should not occur
#'   within field values.
#'
#' @return data frame with one row for each bibliographic item record and one
#'   column for each field. Missing values are represented with \code{NA}.
#'
#' @seealso \code{\link{read_ris}}
#' @export
#'
spread_ris <- function (x, multi_sep=";;") {
    sm <- lazyeval::interp(~ stringr::str_c(value, collapse=x), x=multi_sep)

    x <- dplyr::summarize_(
        dplyr::group_by_(x, ~ id, ~ field),
        value= sm
    )
    tidyr::spread_(ungroup(x), "field", "value", fill=NA)

}

#' Read in a CSV file of converted RIS data
#'
#' Deprecated. Use \code{\link{read_ris}} unless the speed gain is important.
#' Read in the result of \code{\link{convert_ris}}. Note that this function
#' returns a \emph{wide}-format rather than long-format data frame and does not
#' add any ID column.
#'
#' @param filename file to read in, produced from \code{\link{convert_ris}}.
#' @param columns RIS fields to retain (NULL to retain all)
#'
#' @return wide format data frame with one row for each item and one column for
#'   each field.
#'
#' @export
read_ris_csv <- function (filename,
                          columns=getOption("mlaibr.ris_keep")) {

    if (length(columns == 0)) {
        # by default, get everything as a string
        ctypes <- readr::cols(.default=readr::col_character())
    } else {
        ctypes <- setNames(rep(list("c"), length(keep_cols)), keep_cols)
        ctypes <- do.call(readr::cols_only, ctypes)
    }
    frm <- withCallingHandlers(
        readr::read_delim(filename, delim=",", quote="\"", escape_double=TRUE,
            col_names=TRUE,
            col_types=ctypes
        ),
        warning=function (w) {
            if (stringr::str_detect(w$message,
                "^The following named parsers don't match the column names")) {
                invokeRestart("muffleWarning")
            }
        }
    )

    # If we are choosing columns, make sure we supply dummy values
    # if they were missing from the input RIS
    if (length(columns) > 0) {
        missing_cols <- setdiff(columns, colnames(frm))
        if (length(missing_cols) > 0) {
            frm[ , missing_cols] <- ""
        }
    }
    frm
}

#' Convert an RIS file to CSV format
#'
#' Deprecated. Use \code{\link{read_ris}} unless the speed gain is important.
#' Convert RIS to CSV using a Python script; the output can be loaded with
#' \code{\link{read_ris_csv}}.  Note that this generates a \emph{wide}-format
#' rather than long-format data frame.
#'
#' @param in_file RIS file to process
#'
#' @param out_file name of CSV file to write. This is passed to
#'   \code{\link[base]{system2}} as the \code{stdout} parameter, so set
#'   \code{out_file=T} to get the output back as the function return value.
#'
#' @return return value from \code{\link[base]{system2}}.
#' @export
#'
convert_ris <- function (in_file, out_file) {
    scpt <- file.path(path.package("mlaibr"), "python", "aggregate.py")
    system2("python", scpt, in_file, stdout=out_file)
}
