# UK2GTFS option treatDatesAsInt

sets/gets a logical value which determines how dates are processed while
building calendar - used for debugging

## Usage

``` r
UK2GTFS_option_treatDatesAsInt(value)
```

## Arguments

- value:

  option value to be set (logical)

## Value

the current option value when called with no arguments, otherwise the
result of setting the option

## Details

In the critical part of timetable building, handling dates as dates is
about half the speed of handling as int so we treat them as integers.
However that's a complete pain for debugging, so make it configurable.
if errors are encountered during the timetable build phase, try setting
this value to FALSE
