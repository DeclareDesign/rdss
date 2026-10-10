# Replication data for Foos, John, Muller, and Cunningham (2021), Journal of Politics (derived from Dataverse 10.7910/DVN/NDPXND)

One row per voter. `treat` is the randomly assigned treatment,
`marked_register_2014` the turnout outcome, and `ward` and `street` (a
hashed identifier) the levels at which voters are clustered.

## Usage

``` r
foos_etal
```

## Format

A data frame with 8,375 rows and 5 columns: `marked_register_2014`,
`ward`, `street`, `treat`, and `weights`.
