# Get the National Operator Codes database

Downloads Traveline's National Operator Codes register and returns one
row per NOC, with the operator, management division and group it belongs
to.

## Usage

``` r
get_noc(
  url = "https://www.travelinedata.org.uk/noc/api/1.0/nocrecords.xml",
  include_ceased = FALSE
)
```

## Arguments

- url:

  character, the NOC API endpoint

- include_ceased:

  logical, keep codes marked as ceased. \`FALSE\` by default; a ceased
  code can still appear in an old feed, but its names are the ones that
  were in use then and are more likely to collide with a current
  operator's.

## Value

a data frame of

- noc:

  National Operator Code, as written in TransXChange

- public_name:

  the trading name the operator is known by

- licence_name:

  the name on its PSV operator's licence

- operator_id:

  Traveline's id for the legal operator. Two NOCs with the same
  \`operator_id\` are one company

- operator_name:

  that operator's name

- division_id, division:

  the management division, one level up

- group_id, group:

  the corporate group, one level up again

## Details

The register is a four level hierarchy: code, operator, management
division, group. Only the operator level means "the same company" -
\`First Leeds\` and \`First Bradford\` share an operator, \`Stagecoach
London\` and \`Stagecoach Yorkshire\` share only a group, and treating a
group as one operator would merge companies that happen to have the same
owner.

The register is a national reference dataset like NaPTAN, and is used
the same way: download it once and pass it to the functions that need
it, rather than re-downloading per call.

## Examples

``` r
if (FALSE) { # \dontrun{
noc <- get_noc()
# the codes one operator is filed under
noc[noc$operator_id == noc$operator_id[noc$noc == "ELBG"], ]
} # }
```
