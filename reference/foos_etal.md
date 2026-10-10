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

## Examples

``` r

estimatr::lm_robust(marked_register_2014 ~ treat, clusters = street, data = foos_etal)
#>              Estimate Std. Error   t value     Pr(>|t|)     CI Lower   CI Upper
#> (Intercept) 0.4724791 0.01513952 31.208324 2.829526e-43  0.442293258 0.50266491
#> treat       0.0340740 0.01842628  1.849206 6.673218e-02 -0.002385131 0.07053312
#>                    DF
#> (Intercept)  71.20713
#> treat       128.15584
```
