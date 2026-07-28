# Import the .CIF file

Import the .CIF file

## Usage

``` r
nptdr2gtfs(
  path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/October-2006.zip",
  silent = FALSE,
  n_files = NULL,
  enhance_stops = TRUE,
  naptan = get_naptan()
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

## Value

A gtfs object: a named list of data frames representing the tables of a
GTFS file

## Details

Imports the CIF file and returns data.frame
