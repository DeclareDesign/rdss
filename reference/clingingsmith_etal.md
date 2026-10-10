# Replication data for Clingingsmith, Khwaja, and Kremer (2009), Quarterly Journal of Economics

From "Estimating the Impact of the Hajj: Religion and Tolerance in
Islam's Global Gathering," The Quarterly Journal of Economics 124(3):
1133-1170. One row per applicant to the Hajj visa lottery. `success` is
1 for lottery winners; the `views_` columns are attitudes toward each
group, and `views` is their sum.

## Usage

``` r
clingingsmith_etal
```

## Format

A data frame with 958 rows and 8 columns: `success`, `views_saudi`,
`views_indonesian`, `views_turkish`, `views_african`, `views_chinese`,
`views_european`, and `views`.

## Examples

``` r

estimatr::difference_in_means(views ~ success, data = clingingsmith_etal)
#> Design:  Standard 
#>          Estimate Std. Error  t value    Pr(>|t|)  CI Lower  CI Upper       DF
#> success 0.4748337  0.1626723 2.918958 0.003594545 0.1555969 0.7940705 954.3044
```
