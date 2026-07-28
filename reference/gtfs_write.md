# Write GTFS

Takes a list of data frames representing the GTFS format and saves them
as GTFS Zip file.

## Usage

``` r
gtfs_write(
  gtfs,
  folder = getwd(),
  name = "gtfs",
  stripComma = FALSE,
  stripTab = FALSE,
  stripNewline = FALSE,
  quote = TRUE
)
```

## Arguments

- gtfs:

  named list of data.frames

- folder:

  folder to save the gtfs file to

- name:

  the name of the zip file without the .zip extension, default "gtfs"

- stripComma:

  logical, should commas be stripped from text, default = FALSE

- stripTab:

  logical, should tabs be stripped from text, default = FALSE

- stripNewline:

  logical, should newlines be stripped from text, default = FALSE

- quote:

  logical, should strings be quoted, default = TRUE, passed to
  data.table::fwrite

## Value

Invisibly returns NULL, called for the side effect of writing a zip file
