# CRAN comments

## R CMD check results

0 errors | 0 warnings | 1-2 notes

* `checking installed package size ... NOTE: installed size is ~45Mb
  (extdata)`. The package uses several large reference datasets (rail network
  geometry, station gazetteers, historic bank holidays) that are versioned
  separately from the code and downloaded into `inst/extdata` by
  `update_data()`; a fresh install without the downloaded data is small.
* `checking for future file timestamps ... NOTE: unable to verify current
  time` appears only on the offline build machine.

## Notes on other automated-check findings

* The Title and Description use the acronyms GTFS, CIF and NPTDR; all three
  are expanded on first use in the Description ("General Transit Feed
  Specification ('GTFS')", "Common Interface File ('CIF')", "National Public
  Transport Data Repository ('NPTDR')").
* `UK2GTFS_option_*()` functions set package-prefixed global options; setting
  the option is the documented purpose of these user-facing functions (the
  previous value is returned invisibly so callers can restore it).
* Most exported functions operate on multi-gigabyte industry data feeds that
  cannot be shipped or downloaded in examples, so their examples are wrapped
  in `\dontrun{}` (e.g. `gtfs_add_railfares()` requires the licensed National
  Rail fares feed).
* The repository's `LICENSE` file is the verbatim GPL-3 text for GitHub's
  benefit; it is excluded from the built package via `.Rbuildignore`.
