# ATOC to GTFS (Network Rail Version)

Convert ATOC CIF files from Network Rail to GTFS

## Usage

``` r
nr2gtfs(
  path_in,
  silent = TRUE,
  ncores = 1,
  locations = "tiplocs",
  agency = "atoc_agency",
  shapes = FALSE,
  working_timetable = FALSE,
  public_only = TRUE
)
```

## Arguments

- path_in:

  Character, path to Network Rail ATOC file
  e.g."C:/input/toc-full.CIF.gz"

- silent:

  Logical, should progress messages be suppressed (default TRUE)

- ncores:

  Numeric, When parallel processing how many cores to use (default 1)

- locations:

  where to get tiploc locations (see details)

- agency:

  where to get agency.txt (see details)

- shapes:

  Logical, should shapes.txt be generated (default FALSE)

- working_timetable:

  Logical, should WTT times be used instead of public times (default
  FALSE)

- public_only:

  Logical, only return calls/services that are for public passenger
  pickup/set down (default TRUE)

## Value

A gtfs list

## Details

Locations

The .msn file contains the physical locations of stations and other
TIPLOC codes (e.g. junctions). However, the quality of the locations is
often poor only accurate to about 1km and occasionally very wrong.
Therefore, the UK2GTFS package contains an internal dataset of the
TIPLOC locations with better location accuracy, which are used by
default.

However you can also specify \`locations = "file"\` to use the TIPLOC
locations in the ATOC data or provide an SF data frame of your own.

Agency

The ATOC files do not contain the necessary information to build the
agency.txt file. Therefore this data is provided with the package. You
can also pass your own data frame of agency information.

## See also

Other main:
[`atoc2gtfs()`](https://itsleeds.github.io/UK2GTFS/reference/atoc2gtfs.md)
