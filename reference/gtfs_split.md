# Split a GTFS object

For large regions GTFS files can get very big. This function splits a
GTFS object into a list of GTFS objects. It tries to balance the sizes
of the objects. Splits are made by agency_id so maximum number of splits
equals the number of unique agency_ids.

## Usage

``` r
gtfs_split(gtfs, n_split = 2)
```

## Arguments

- gtfs:

  a gtfs object

- n_split:

  how many to split into default 2

## Value

a list of GTFS objects
