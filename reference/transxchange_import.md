# Import a TransXchange XML file

Import a TransXchange XML file

## Usage

``` r
transxchange_import(file, run_debug = TRUE, full_import = FALSE)
```

## Arguments

- file:

  character, path to an XML file e.g. "C:/data/file.xml"

- run_debug:

  logical, if TRUE extra checks are performed, default TRUE

- full_import:

  logical, if false data no needed for GTFS is excluded

## Value

named list of data frames, one per TransXchange section (or NULL if the
file contains no services)

## Details

This function imports the raw transXchange XML files and converts them
to a R readable format.
