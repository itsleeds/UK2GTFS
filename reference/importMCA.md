# Import the .mca file

Import the .mca file

## Usage

``` r
importMCA(
  file,
  silent = TRUE,
  ncores = 1,
  full_import = FALSE,
  working_timetable = FALSE,
  public_only = TRUE
)
```

## Arguments

- file:

  Path to .mca file

- silent:

  logical, should messages be displayed

- ncores:

  number of cores to use when parallel processing

- full_import:

  import all data, default FALSE

- working_timetable:

  use rail industry scheduling times instead of public times

- public_only:

  only return calls that are for public passenger pick up/set down

## Value

named list of data tables: stop_times and schedule, plus (when
full_import is TRUE) TI, TA, TD, AA, and CR records

## Details

Imports the .mca file and returns data.frame
