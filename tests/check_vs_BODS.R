# Check Against the BODS Converter
# Get Transxchange files from the BODS archive and convert them to GTFS using the UK2GTFS package. 
# Then write the resulting GTFS data to a specified folder.

# Can then check against BODS own GTFS files.

library(UK2GTFS)
path_in <- "../UK2GTFS-BODS-check/bodds_archive_20260702_121mXBC"

# This is a manual comparison script, not an automated test: it needs a local
# copy of the BODS archive. Exit quietly when the data is not present, so that
# tools that execute everything in tests/ (e.g. covr) do not fail.
# (R CMD check already skips this file via .Rbuildignore.)
if (!dir.exists(path_in)) {
  message("check_vs_BODS.R skipped: ", path_in, " not found")
  quit(save = "no", status = 0)
}

fls = list.files(path_in, full.names = TRUE, recursive = TRUE, pattern = ".xml")

# filter_duplicate_files = TRUE removes superseded versions of the same
# service that accumulate in the BODS change archive (see ?txc_filter_files).
# Without it, revised timetables are double-counted. Set filter_date to a date
# inside the period you want to analyse.
gtfs <- transxchange2gtfs(path_in = fls,
          ncores = 20, force_merge = TRUE, silent = FALSE,
          filter_duplicate_files = TRUE,
          filter_date = as.Date("2026-07-08"))

gtfs_write(gtfs, 
           folder = "../UK2GTFS-BODS-check/",
           name = "gtfs_EA")
