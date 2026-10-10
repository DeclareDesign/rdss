# Replication data for Bonilla and Tillery (2020), American Political Science Review (obtained from Dataverse 10.7910/DVN/IUZDQI)

One row per survey respondent. `Z` is the randomly assigned message (a
factor with four levels) and `blm_support` the outcome; the remaining
columns are respondent characteristics.

## Usage

``` r
bonilla_tillery
```

## Format

A data frame with 849 rows and 10 columns: `female`, `lgbtq`, `age`,
`religiosity`, `income`, `college`, `blm_familiarity`, `linked_fate`,
`Z`, and `blm_support`.

## Examples

``` r

estimatr::lm_robust(blm_support ~ Z, data = bonilla_tillery)
#>                    Estimate Std. Error   t value      Pr(>|t|)    CI Lower
#> (Intercept)      0.84191176 0.01531579 54.970183 2.731468e-281  0.81185031
#> Znationalism    -0.01181468 0.02125833 -0.555767  5.785173e-01 -0.05354000
#> Zfeminism       -0.03623491 0.02203460 -1.644455  1.004543e-01 -0.07948389
#> Zintersectional -0.03714986 0.02247282 -1.653102  9.868161e-02 -0.08125896
#>                    CI Upper  DF
#> (Intercept)     0.871973220 845
#> Znationalism    0.029910644 845
#> Zfeminism       0.007014069 845
#> Zintersectional 0.006959240 845
```
