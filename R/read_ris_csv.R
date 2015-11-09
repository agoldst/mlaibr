#' Read in a CSV file of converted RIS data
#'
#' Deprecated. Use \code{\link{read_ris_files}} unless the speed gain is important.
#' Read in the result of \code{\link{convert_ris}}. Note that this function returns a \emph{wide}-format rather than long-format data frame and does not add any ID column.
#'
#' @param filename file to read in, produced from \code{\link{convert_ris}}.
#' @param columns RIS fields to retain (NULL to retain all)
#' 
#' @return wide format data frame with one row for each item and one column for each field.
#'
#' @export 
read_ris_csv <- function (filename,
                          columns=options("mlaib.ris_keep")) {

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
#' Deprecated. Use \code{\link{read_ris_files}} unless the speed gain is important.
#' Convert RIS to CSV using a Python script; the output can be loaded with \code{\link{read_ris_csv}}.  Note that this generates a \emph{wide}-format rather than long-format data frame.
#'
#' @param in_file RIS file to process
#'
#' @param out_file name of CSV file to write. This is passed to \code{\link[base]{system2}} as the \code{stdout} parameter, so set \code{out_file=T} to get the output back as the function return value.
#'
#' @return return value from \code{\link[base]{system2}}.
#' @export
#'
convert_ris <- function (in_file, out_file) {
    scpt <- file.path(path.package("mlaib"), "python", "aggregate.py")
    system2("python", scpt, in_file, stdout=out_file)
}

#' Read RIS into a Data Frame
#'
#' Load an exported RIS data file into a data frame. Records are assigned an arbitrary sequential ID number (corresponding to the ordering of results from the database search query). The result is not quite "tidy," because no special parsing is done to extract information within the catch-all \code{N1} field; use \code{\link{N1_field}} for that. Note also that all fields remain in string format; for example, dates are not parsed (see \code{\link{Y1_date}}).
#'
#' @param filename Name of an RIS file to read.
#' @return a \code{\link[dplyr]{tbl_df}} with one row for each field and three columns, \code{id}, \code{field} (the RIS field code), and \code{value}.

#'
#' @export 
read_ris <- function (filename) {
    ll <- readLines(filename, encoding="UTF-8")
    ers <- stringr::str_detect(ll, "^ER  -")
    result <- data.frame(id=cumsum(ers) + 1, value=ll,
                         stringsAsFactors=FALSE)
    result <- result[!ers, ]
    result <- result[result$value != "", ]
    sp <- stringr::str_split(result$value, coll("  -"), n=2)
    result$field <- vapply(sp, `[[`, "", 1)
    result$value <- stringr::str_trim(vapply(sp, `[[`, "", 2))
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

    dplyr::tbl_df(result)
}

#' Convert bibliographic records from long to wide format
#'
#' The data frame from \code{\link{read_ris_files}} has many rows for each bibliographic record. Many questions relate to bibliographic items as a unit, in which case it is convenient to have a data frame in the corresponding wide form. However, note that this is a somewhat non-normalized format since many RIS fields can repeat within a record. Repeat fields are joined into a single string value.
#'
#' It would be a little more formal to produce list columns from repeating fields, but that's just a drag. It may make more sense to normalize by hand by making separate tables of the repeating columns and keying to ID (or accession number), then spreading out only the single columns of interest here.
#'
#' @param x data frame with \code{id,field,value} columns
#' @param multi_sep separator string for repeated fields. Should not occur within field values.
#'
#' @return data frame with one row for each bibliographic item record and one column for each field. Missing values are represented with \code{NA}.
#'
#' @seealso \code{\link{read_ris_files}}
#' @export
#'
spread_ris <- function (x, multi_sep=";;") {
    x <- dplyr::summarize_(
        dplyr::group_by_(x, ~ id, ~ field),
        field= ~ stringr::str_c(value, collapse=multi_sep)
    )
    tidyr::spread_(x, "id", "field", fill=NA)

}

#' Read RIS files into a long-format data frame
#' 
#' This function loads one or more RIS files into a long data frame. It assigns unique record ID numbers and optionally filters out unwanted fields. The result has one line for each record-field combination, including possible repeats when a record has multiple instances of a field. To project to a data frame with one row for each bibliographic record, use \code{\link{spread_ris}}.
#'
#' @param filenames vector of RIS files to read.
#' @param fields which RIS fields to keep in the result. A default list is set by the package option \code{mlaib.ris_keep}. To keep all fields, set \code{fields=NULL}. 
#'
#' @return a data frame with columns \code{id,field,value}
#'
#' @seealso \code{\link{spread_ris}}
#' @export
#'
read_ris_files <- function (filenames, fields=options("mlaib.ris_keep")) {
    result <- vector("list", length(filenames))
    base_id <- 0

    if (length(fields) > 0) {
        flt <- lazyeval::interp(~ field %in% x, x=fields)
    } else {
        flt <- ~ TRUE
    }
    for (i in seq_along(filenames)) {
        frm <- read_ris(filenames[[i]])
        frm <- dplyr::filter_(frm, flt)
        frm$id <- frm$id + base_id
        result[[i]] <- frm
        base_id <- base_id + tail(frm$id, 1)
    }


    dplyr::bind_rows(result)
}


