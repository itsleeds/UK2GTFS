# as data table naptan stop point

Unpacks selected naptan XML doc elements into data.table

## Usage

``` r
as_data_table_naptan_stop_point(doc, stopTypes = c("RLY"))
```

## Arguments

- doc:

  xml document node

- stopTypes:

  list of stop types to restrict processing to (defaults to railway
  station)

## Value

data table of stop points

## Details

RLY stop types include TIPLOC & CRS fields. The quality of the
geographic location is better than from BPLAN
