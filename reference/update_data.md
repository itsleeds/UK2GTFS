# Update the data inside the UK2GTFS package

As UK2GTFS has large datasets that update separately to the R package
they are checked and downloaded at package load time. This function
checks for and downloaded any updated to the data.

## Usage

``` r
update_data(timeout = 60)
```

## Arguments

- timeout:

  maximum duration (in seconds) to wait for a response from the server
  (github.com)

## Value

Invisibly returns NULL, called for the side effect of updating the
package data

## Details

Raw data can be viewed and contributed to at
https://github.com/ITSLeeds/UK2GTFS-data
