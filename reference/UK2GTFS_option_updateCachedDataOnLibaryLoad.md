# UK2GTFS option updateCachedDataOnLibaryLoad

sets/gets a logical value which determines if the data cached in the
library is checked for update when loaded

## Usage

``` r
UK2GTFS_option_updateCachedDataOnLibaryLoad(value)
```

## Arguments

- value:

  option value to be set (logical)

## Value

the current option value when called with no arguments, otherwise the
result of setting the option

## Details

when child processes are initialised we want to suppress this check, so
it is also used for that purpose
