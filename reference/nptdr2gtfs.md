# Import Nataional Public Transport Data Repository (NPTDR) data as GTFS

Import Nataional Public Transport Data Repository (NPTDR) data as GTFS

## Usage

``` r
nptdr2gtfs(
  path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/October-2006.zip",
  silent = FALSE,
  n_files = NULL,
  enhance_stops = TRUE,
  naptan = get_naptan(),
  year = nptdr_archive_year(path)
)
```

## Arguments

- path:

  Path to zipped folder for NPTDR data

- silent:

  Logical, should messages be returned

- n_files:

  debug option numerical vector for files to be passed e.g. 1:10

- enhance_stops:

  Logical, if TRUE will download current NaPTAN to add in any missing
  stops

- naptan:

  Naptan Locations from get_naptan()

- year:

  Year of the NPTDR archive, used to apply standard_mode_overrides()
  rules

## Value

A gtfs object: a named list of data frames representing the tables of a
GTFS file

## Details

NPTDR is a UK-wide dataset of timetables, and is available from
data.gov.uk. The data is distributed as a zip file containing a number
of CIF files, one for each local authority area. The function imports
the CIF files and returns a GTFS object.
