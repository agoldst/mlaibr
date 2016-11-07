This small R package supplies some functions for working with bibliographic data  in [RIS](https://en.wikipedia.org/wiki/RIS_(file_format)) format from the [MLA International Bibliography](http://www.mla.org/bibliography). The functions have online help accessible in the usual way, which gives a little more detail.

# Installation

The easiest installation method requires the [devtools](http://cran.r-project.org/web/packages/devtools/) package:

```R
install.packages("devtools")  # only if not already installed
devtools::install_github("agoldst/mlaibr")
```

This will also require the packages this package depends on. Note that some of these dependencies, like [dplyr](http://cran.r-project.org/web/packages/dplyr/), must normally be built from source and thus require a compiler.

# Usage

I plan to add a tutorial vignette eventually.

Read in RIS data with `read_ris("file.ris")`. A vector of filenames may be supplied. RIS files compressed with zip can also be specified (`read_ris("file.zip")`). Note that by default not all RIS fields will be read in; use `read_ris("file.ris", fields=NULL)` to get them all. 

The result is a "long" data frame, where each row corresponds to a single piece of bibliographic information, and each bibliography record has many rows (all with the same arbitrary ID assigned by `read_ris`). This form, known as "tidy" data, is efficient for some processing tasks but often quite inconvenient for further manipulation. To work with a "wide" data frame, use `spread_ris`.

`Y1_year` converts the `Y1` field to a publication year in `Date` format (but note this discards any finer-grained date information that may also be present in that field).

In the MLAIB, important supplemental information is contained in the `N1` field, including the bibliography's own record ID numbers ("accession numbers"). See `?N1_field` for help extracting this information.

Finally, I include a couple of some utilities for dealing with MLAIB subject headings, which are found in `KW` fields (many for each record). `strip_subject_relation` is for removing relation terms ("compared to," etc.). `is_author` uses the presence of birth-death dates as a simple heuristic for detecting personal names as subject headings (but works with a data range are misidentified as persons).

# System requirements

Automatic handling of zip files depends on R's zip support, which is wobbly on Windows. 

Though it is listed as a requirement in the DESCRIPTION file, Python is *not* required to use this package. It is only a requirement for a now-deprecated alternative way of loading data, which uses a Python 2 script to convert the RIS file to a CSV, then loads that CSV. If you want to experiment with this, see `convert_ris` and `read_ris_csv`. It might gain you a little speed or memory efficiency, but no promises. I don't recommend this. `read_ris` is what I rely on.

# The usual provisos

I make no guarantees about how well this works. I am literature scholar who programs for reasons that are often unclear even to me. I am a member of the Modern Language Association but neither the MLA nor anyone else is affiliated with this programming project. I am releasing this source code under the permissive MIT license. If you copy or modify this code, please attribute the parts by me to me.

I am happy to hear from anyone who makes use of this, and I'd be grateful to learn of bugs via the [issue tracker](http://github.com/agoldst/mlaibr/issues). I cannot promise to support anyone in using this package.
