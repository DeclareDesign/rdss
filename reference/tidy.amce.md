# Tidy estimates from the amce estimator

Runs amce estimation function and returns tidy data frame output

## Usage

``` r
# S3 method for class 'amce'
tidy(x, alpha = 0.05, ...)
```

## Arguments

- x:

  an amce fit object from cjoint::amce

- alpha:

  Confidence level

- ...:

  Extra arguments to pass to tidy

## Value

a data.frame of estimates

## Details

See
https://book.declaredesign.org/experimental-descriptive.html#conjoint-experiments

## Examples

``` r

# \donttest{
library(cjoint)
#> Loading required package: sandwich
#> Loading required package: lmtest
#> Loading required package: zoo
#> 
#> Attaching package: ‘zoo’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.Date, as.Date.numeric
#> Loading required package: survey
#> Loading required package: grid
#> Loading required package: Matrix
#> Loading required package: survival
#> 
#> Attaching package: ‘survey’
#> The following object is masked from ‘package:graphics’:
#> 
#>     dotchart
#> cjoint: AMCE Estimator for Conjoint Experiments
#> Version: 2.1.3
#> Authors: Soubhik Barari [aut],
#>   Elissa Berwick [aut],
#>   Jens Hainmueller [aut],
#>   Daniel Hopkins [aut],
#>   Sean Liu [aut],
#>   Anton Strezhnev [aut, cre],
#>   Teppei Yamamoto [aut]

data(immigrationconjoint)
data(immigrationdesign)

# Run AMCE estimator using all attributes in the design
results <- amce(Chosen_Immigrant ~  Gender + Education + `Language Skills` +
                  `Country of Origin` + Job + `Job Experience` + `Job Plans` +
                  `Reason for Application` + `Prior Entry`, data = immigrationconjoint,
                cluster = TRUE, respondent.id = "CaseID", design = immigrationdesign)

# Print summary
# tidy(results)
# }
```
