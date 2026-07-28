# Export ATOC stations as GTFS stops.txt

Export ATOC stations as GTFS stops.txt

## Usage

``` r
station2stops(station, TI)
```

## Arguments

- station:

  station SF data frame from the importMSN function

- TI:

  TI object from the importMCA function

## Value

named list with a stops data frame (GTFS stops.txt format) and a TIPLOC
to CRS lookup table

## Details

Export ATOC stations as GTFS stops.txt
