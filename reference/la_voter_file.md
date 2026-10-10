# Voter file sample for Los Angeles County

Party registration, age, census tract, and 2012 turnout for 1,000
registered voters sampled at random from the Los Angeles County voter
file. One row per voter.

## Usage

``` r
la_voter_file
```

## Format

A data frame with 1000 rows and 4 variables:

- party:

  political party registration

- age:

  age of voter in years

- census_tract:

  US Census tract number

- voted_2012:

  1 if the voter voted in the 2012 general election, 0 otherwise

## Source

California Secretary of State.

## Examples

``` r

table(la_voter_file$party)
#> 
#>  AI AME DEM  DS  G3 GRN  IR LIB NAT  NP NPP  PF REP 
#>  30   1 509 148   1   5   3   6   1   1 100  11 184 
```
