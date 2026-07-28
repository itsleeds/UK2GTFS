# Download Timetable from National Rail Data Portal

Downloads ATOC CIF timetables from https://opendata.nationalrail.co.uk

## Usage

``` r
nrdp_timetable(
  destfile = "timetable.zip",
  username = Sys.getenv("NRDP_username"),
  password = Sys.getenv("NRDP_password"),
  url = "https://opendata.nationalrail.co.uk/api/staticfeeds/3.0/timetable"
)
```

## Arguments

- destfile:

  Destination path and name of the zip file

- username:

  your username

- password:

  your password

- url:

  URL of data source

## Value

Invisibly returns NULL, called for the side effect of downloading a file
