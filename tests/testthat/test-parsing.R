context("Parsing the data")

test_data <- c(
"TY  - JOUR
AU  - Hutchison, Percy Adams
T1  - Poetry, Philosophy, and Religion
JO  - PMLA
Y1  - 1907
VL  - 22
IS  - 4
SP  - 697
EP  - 706
N1  - Accession Number: 0000209451. Peer Reviewed: Yes. Publication Type: journal article. Language: English. Update Code: 000004. Sequence No: 0000-1-3691. 
KW  - English literature
KW  - 1500-1899
KW  - poetry
KW  - relationship to philosophy
KW  - religion
ER  - 

TY  - JOUR
AU  - Dykhuizen, George
T1  - John Dewey: The Chicago Years
JO  - Journal of the History of Philosophy
Y1  - 1964/10
VL  - 2
IS  - 2
SP  - 227
EP  - 253
N1  - Accession Number: 2010392156. Publication Type: journal article. Language: English. Update Code: 201001. Sequence No: 2010-1-14038. DOI: 10.1353/hph.2008.1073. 
KW  - American literature
KW  - 1900-1999
KW  - Dewey, John (1859-1952)
KW  - prose
KW  - treatment of philosophy
KW  - education
KW  - relationship to pragmatism
KW  - University of Chicago
ER  - 
")

tfile <- tempfile()
writeLines(test_data, tfile)
b <- read_ris(tfile) |> spread_ris()

test_that("N1_field works", {
    expect_equal(N1_field(b$N1, "Accession Number"),
                 c("0000209451", "2010392156"))
    expect_equal(N1_field(b$N1, "Peer Reviewed"),
                 c("Yes", NA))
    expect_equal(N1_field(b$N1, "DOI"), c(NA, "10.1353/hph.2008.1073"))
})

test_that("Year extraction works", {
    expect_equal(Y1_year(b$Y1),
                as.Date(c("1907-01-01", "1964-01-01")))
})

if (file.exists(tfile)) {
    unlink(tfile)
}
