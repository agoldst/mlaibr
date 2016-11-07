This small R package supplies some functions for working with bibliographic data  in [RIS](https://en.wikipedia.org/wiki/RIS_(file_format)) format from the [MLA International Bibliography](http://www.mla.org/bibliography). The functions have online help accessible in the usual way, which gives a little more detail.

I plan to add a tutorial vignette eventually.

Read in RIS data with `read_ris("file.ris")`. A vector of filenames may be supplied. Note that by default not all RIS fields will be read in; use `read_ris("file.ris", fields=NULL)` to get them all. 

The result is a "long" data frame, where each row corresponds to a single piece of bibliographic information, and each bibliography record has many rows (all with the same arbitrary ID assigned by `read_ris`). This form, known as "tidy" data, is efficient for some processing tasks but often quite inconvenient for further manipulation. To work with a "wide" data frame, use `spread_ris`.

`Y1_year` converts the `Y1` field to a publication year in `Date` format (but note this discards any finer-grained date information that may also be present in that field).

In the MLAIB, important supplemental information is contained in the `N1` field, including the bibliography's own record ID numbers ("accession numbers"). See `?N1_field` for help extracting this information.

Finally, I include a couple of some utilities for dealing with MLAIB subject headings, which are found in `KW` fields (many for each record). `strip_subject_relation` is for removing relation terms ("compared to," etc.). `is_author` uses the presence of birth-death dates as a simple heuristic for detecting personal names as subject headings (but works with a data range are misidentified as persons).


