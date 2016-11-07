test_data <- c(
"T1  - Shelley's On Life
KW  - English literature
KW  - 1800-1899
KW  - Shelley, Percy Bysshe (1792-1822)
KW  - 'On Life'
KW  - 'Hymn to Intellectual Beauty'
KW  - prose
KW  - essay
KW  - treatment of beauty
KW  - relationship to philosophy
KW  - artistic development
KW  - Christianity
KW  - compared to Wordsworth, William (1770-1850): 'Intimations of Immortality from Recollections of Early Childhood'
KW  - Drummond, Sir William (1770?-1828)
ER  - 

T1  - Thoreau's Philosophical Apprenticeship
KW  - American literature
KW  - 1800-1899
KW  - Thoreau, Henry David (1817-1862)
KW  - prose
KW  - role of philosophy
KW  - at Harvard University
KW  - source study
ER  - 

T1  - Fluid Symbols in American Modernism: William James, Gertrude Stein, George Santayana, and Wallace Stevens
KW  - American literature
KW  - 1900-1999
KW  - Stein, Gertrude (1874-1946)
KW  - Santayana, George (1863-1952)
KW  - Stevens, Wallace (1879-1955)
KW  - Modernism
KW  - symbolism
KW  - treatment of consciousness
KW  - sources in James, William (1842-1910)
KW  - The Principles of Psychology (1890)
ER  - 
")

tfile <- tempfile()
writeLines(test_data, tfile)
b <- read_ris(tfile)
s <- subjects_frame(b)

test_that("relation term stripping works", { 
    expect_equal(
        strip_subject_relation("American literature"),
        "American literature",
        info="If no relation terms, should be no-op")

    expect_equal(
        strip_subject_relation(
            "sources in James, William (1842-1910)"
        ), "James, William (1842-1910)",
        info="Removing one relation term"
    )
    expect_equal(
        strip_subject_relation(
"compared to Wordsworth, William (1770-1850): 'Intimations of Immortality from Recollections of Early Childhood'"
        ), "Wordsworth, William (1770-1850)",
        info="removing relation term and subheading"
    )

    trouble <- "discusses theories of relationship to realism"
    expect_equal(strip_subject_relation(trouble), "realism")
})

test_that("subject frame generation works", {
    expect_equal(colnames(s), c("id", "value"))
    expect_equal(s$id, rep(1L:3L, times=c(13, 7, 10)))

    expect_equal(s$value[1:13],
        c("English literature", "1800-1899",
          "Shelley, Percy Bysshe (1792-1822)", "'On Life'",
          "'Hymn to Intellectual Beauty'", "prose", "essay", "beauty", 
          "philosophy", "artistic development", "Christianity",
          "Wordsworth, William (1770-1850)",
          "Drummond, Sir William (1770?-1828)"),
        info="Spot check of subject-relation stripping")
})

test_that("author detection works", {
    expect_true(is_author("Shakespeare, William (1564-1616)"))
    expect_true(is_author("Drummond, Sir William (1770?-1828)"))
    expect_true(is_author("Coetzee, J. M. (1940-  )"))
    expect_true(is_author("Boccaccio, Giovanni (ca. 1313-1375)"))
    
    # and the known failure mode
    expect_true(is_author("A la recherche du temps perdu (1913-1929)"),
                info="Known error with titles that have date ranges")
})

test_that("subject author extraction works", {
    sa <- s %>%
        dplyr::filter(is_author(value)) %>%
        dplyr::mutate(value=subject_author(value))
    expect_equal(sa, dplyr::data_frame(
        id=as.numeric(rep(1:3, times=c(3, 1, 4))),
        value=c(
            "Shelley, Percy Bysshe",
            "Wordsworth, William",
            "Drummond, Sir William",
            "Thoreau, Henry David",
            "Stein, Gertrude",
            "Santayana, George",
            "Stevens, Wallace",
            "James, William"
        )
    ))
})

test_that("last names are extracted correctly", {
    sa <- s %>%
        dplyr::filter(is_author(value)) %>%
        dplyr::mutate(value=subject_author(value))
    expect_equal(subject_author_last(sa$value),
        c("shelley", "wordsworth", "drummond", "thoreau", "stein",
          "santayana", "stevens", "james")
    )
})

if (file.exists(tfile)) {
    unlink(tfile)
}
