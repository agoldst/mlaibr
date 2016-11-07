library(dplyr)

test_data <- c(
"TY  - JOUR
AU  - Hutchison, Percy Adams
T1  - Poetry, Philosophy, and Religion
JO  - PMLA
Y1  - 1907
ER  - 

TY  - JOUR
AU  - Fletcher, Jefferson B.
T1  - Dante's 'Second Love'
JO  - Modern Philology
Y1  - 1915/07
KW  - Italian literature
KW  - Vita nuova
ER  - 
")

test_target <- dplyr::data_frame(
    id=as.numeric(rep(1:2, times=c(5, 7))),
    field=c("TY", "AU", "T1", "JO", "Y1", "TY", "AU",
            "T1", "JO", "Y1", "KW", "KW"),
    value=c("JOUR", "Hutchison, Percy Adams",
            "Poetry, Philosophy, and Religion",
            "PMLA", "1907", "JOUR", "Fletcher, Jefferson B.",
            "Dante's 'Second Love'", "Modern Philology", "1915/07",
            "Italian literature", "Vita nuova")
)

tfile <- tempfile()
writeLines(test_data, tfile)
tfile2 <- tempfile()
writeLines(test_data, tfile2)

test_that("loading RIS data files works", {
    mlaib:::read_ris_file(tfile) %>%
        expect_equal(test_target, info="Loading single file")
    
    read_ris(tfile) %>%
        expect_equal(test_target,
                     info="Loading single file with read_ris")
    
    double_target <- dplyr::bind_rows(test_target, test_target) %>%
        dplyr::mutate(id=id + rep(c(0,2), each=12))

    read_ris(c(tfile, tfile)) %>% 
        expect_equal(double_target, info="Loading multiple files")

    read_ris(c(tfile, tfile2), src_labels=c("a", "b")) %>%
        expect_equal(double_target %>% bind_rows(
            data_frame(id=1:4, field="src", value=rep(c("a", "b"), each=2))
            ) %>% arrange(id),
                     info="loading with src field")

    con <- file(tfile, "r")
    read_ris(con) %>%
        expect_equal(test_target,
                     info="loading from one connection")
    close(con)
    cons <- lapply(c(tfile, tfile2), file, "r")
    read_ris(cons) %>%
        expect_equal(double_target,
                     info="loading from two connections")
    lapply(cons, close)

    restricted_fields <- c("TY", "AU", "T1", "KW")
    read_ris(c(tfile, tfile), fields=restricted_fields) %>% 
        expect_equal(
            double_target %>%
                dplyr::filter(field %in% restricted_fields) %>%
                dplyr::mutate(id=as.numeric(rep(1:4, times=c(3, 5, 3, 5)))),
            info="Load file with field restriction"
        )
})

test_that("spreading RIS data works", {
    read_ris(tfile) %>%
        spread_ris() %>%
        expect_equal(dplyr::data_frame(
            id=as.numeric(1:2),
            TY=rep("JOUR", 2),
            AU=c("Hutchison, Percy Adams", "Fletcher, Jefferson B."),
            T1=c("Poetry, Philosophy, and Religion", "Dante's 'Second Love'"),
            JO=c("PMLA", "Modern Philology"),
            Y1=c("1907", "1915/07"), 
            KW=c(NA, "Italian literature;;Vita nuova")
        ))
})

        
for (f in c(tfile, tfile2)) {
    if (file.exists(f))
        unlink(f)
}
