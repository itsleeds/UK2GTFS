# UK2GTFS option stopProcessingAtUid

sets/gets a UID value at which processing will stop - used for debugging

## Usage

``` r
UK2GTFS_option_stopProcessingAtUid(value)
```

## Arguments

- value:

  option value to be set (char)

## Value

the current option value when called with no arguments, otherwise the
result of setting the option

## Details

If no value passed in will return the current setting of the option.
(Usually NULL) If value passed in, timetable build processing will stop
in atoc_overlay.makeCalendarInner() when an exact match for that value
is encountered.

THIS ONLY WORKS WITH ncores==1
