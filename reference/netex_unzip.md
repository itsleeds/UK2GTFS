# Extract NeTEx fare files from a BODS archive

BODS fare archives are zips of zips: a top level archive contains one
folder per operator, and each operator folder contains either raw
\`.xml\` NeTEx files or further \`.zip\` files that themselves contain
\`.xml\` files. This function unpacks an archive (recursively, following
nested zips) and returns the paths of every extracted \`.xml\` file.

## Usage

``` r
netex_unzip(path, exdir = tempfile("netex_"), pattern = NULL)
```

## Arguments

- path:

  path to a \`.zip\` archive, or a folder already containing NeTEx files
  / nested zips.

- exdir:

  directory to extract into. Defaults to a new temporary folder. Keep
  this short on Windows (see Details).

- pattern:

  optional regular expression; only entries whose name matches are
  extracted from the top-level archive, and only matching \`.xml\` paths
  are returned (e.g. \`"Beestons"\` to restrict to one operator).

## Value

a character vector of paths to extracted \`.xml\` files.

## Details

Nested archives are extracted into short, sequentially-named sub-folders
(\`z00001\`, \`z00002\`, ...) directly under \`exdir\` rather than
mirroring the original deep folder structure. BODS NeTEx file names are
very long (well over 100 characters) and, combined with long operator
folder names, easily exceed the Windows 260-character path limit, which
silently breaks extraction. Flattening into short sub-folders keeps
every path short while still keeping each archive's files apart. For the
best chance of success on Windows, keep \`exdir\` itself short (the
default temp folder is short).
