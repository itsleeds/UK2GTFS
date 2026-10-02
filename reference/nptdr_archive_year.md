# Which NPTDR archive a path names

Which NPTDR archive a path names

## Usage

``` r
nptdr_archive_year(path)
```

## Arguments

- path:

  path to an NPTDR archive zip

## Value

the year as a character scalar, or NA

## Details

The archives are distributed as "October-2006.zip" and similar, so the
year is in the file name. A few of the mode rules are gated on it,
because NPTDR reassigns its numeric operator codes between archives.
Returns NA when the name carries no year, which leaves those rules
switched off rather than applied to an archive they were not measured
against.
