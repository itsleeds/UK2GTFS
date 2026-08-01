# Interpolate stop times

Sometimes bus timetables do not give unique stop times to every stop.
Instead several stops in an row are given the same stop time. This
function interpolates the stop times so that each bust stop is given a
unique arrival and departure time.

## Usage

``` r
gtfs_interpolate_times(gtfs, ncores = 1)
```

## Arguments

- gtfs:

  named list of data.frames

- ncores:

  unused, kept for backwards compatibility. The interpolation is fully
  vectorised and runs single-threaded; the old per-trip parallel
  implementation spent most of its time splitting millions of trips into
  individual data frames and doing lubridate Period (S4) arithmetic on
  each, which was slower on 10 cores than this version on one.

## Value

a gtfs object with interpolated stop times

## Details

Note this is not possible if the final arrival time is a duplicated
time, in which case the times are unmodified. Interpolation is based on
arrival time only. If after interpolation departure time is less than
arrival time then departure time is set to arrival time.
