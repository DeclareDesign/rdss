# Tidy estimates from the amce estimator

A [`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
method for fits from
[`cjoint::amce()`](https://rdrr.io/pkg/cjoint/man/amce.html): one row
per attribute level, with normal-approximation confidence intervals.

## Usage

``` r
# S3 method for class 'amce'
tidy(x, alpha = 0.05, ...)
```

## Arguments

- x:

  A fit from
  [`cjoint::amce()`](https://rdrr.io/pkg/cjoint/man/amce.html).

- alpha:

  The significance level for the confidence intervals. Defaults to 0.05,
  which gives 95 percent intervals.

- ...:

  Not used.

## Value

A data frame with columns `attribute`, `level`, `estimate`, `std.error`,
`statistic`, `p.value`, `conf.low`, and `conf.high`.

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

tidy(results)
#>                 attribute                     level     estimate   std.error
#> 1       Country of Origin                   Germany  0.047160626 0.016671472
#> 2       Country of Origin                    France  0.026912000 0.017382320
#> 3       Country of Origin                    Mexico  0.010474179 0.017579179
#> 4       Country of Origin               Philippines  0.034025766 0.017482232
#> 5       Country of Origin                    Poland  0.032579818 0.017653990
#> 6       Country of Origin                     China -0.011254307 0.019551176
#> 7       Country of Origin                     Sudan -0.051811584 0.019958733
#> 8       Country of Origin                   Somalia -0.053097324 0.019408848
#> 9       Country of Origin                      Iraq -0.112660797 0.020362819
#> 10              Education                 4th grade  0.033068508 0.015010234
#> 11              Education                 8th grade  0.057744013 0.015013278
#> 12              Education               high school  0.119483476 0.015154028
#> 13              Education          two-year college  0.148820530 0.017368527
#> 14              Education            college degree  0.180076992 0.017560545
#> 15              Education           graduate degree  0.176068029 0.016749075
#> 16                 Gender                      male -0.026023159 0.008040397
#> 17                    Job                    waiter -0.006814709 0.016914981
#> 18                    Job       child care provider  0.014886098 0.016865958
#> 19                    Job                  gardener  0.013171373 0.016940321
#> 20                    Job         financial analyst  0.063934683 0.029772752
#> 21                    Job       construction worker  0.037824466 0.016877341
#> 22                    Job                   teacher  0.073287616 0.016814161
#> 23                    Job       computer programmer  0.079101210 0.028607217
#> 24                    Job                     nurse  0.084736815 0.016450766
#> 25                    Job        research scientist  0.127636596 0.028625588
#> 26                    Job                    doctor  0.157302433 0.028799940
#> 27         Job Experience                 1-2 years  0.065290374 0.011059530
#> 28         Job Experience                 3-5 years  0.107817867 0.011603473
#> 29         Job Experience                  5+ years  0.113148256 0.011395097
#> 30              Job Plans    contract with employer  0.124929937 0.011723682
#> 31              Job Plans  interviews with employer  0.025217294 0.011811404
#> 32              Job Plans no plans to look for work -0.157301077 0.011785335
#> 33        Language Skills            broken English -0.056319723 0.011352484
#> 34        Language Skills  tried English but unable -0.126359527 0.011409971
#> 35        Language Skills          used interpreter -0.159740917 0.011629605
#> 36            Prior Entry           once as tourist  0.055954954 0.012506698
#> 37            Prior Entry     many times as tourist  0.054748425 0.012957702
#> 38            Prior Entry    six months with family  0.075317887 0.012647739
#> 39            Prior Entry    once w/o authorization -0.110084275 0.013072266
#> 40 Reason for Application           seek better job -0.038318972 0.008978289
#> 41 Reason for Application        escape persecution  0.055634454 0.016908419
#>      statistic      p.value      conf.low   conf.high
#> 1    2.8288220 4.671968e-03  0.0144851413  0.07983611
#> 2    1.5482398 1.215646e-01 -0.0071567208  0.06098072
#> 3    0.5958286 5.512897e-01 -0.0239803797  0.04492874
#> 4    1.9463056 5.161804e-02 -0.0002387791  0.06829031
#> 5    1.8454648 6.496995e-02 -0.0020213667  0.06718100
#> 6   -0.5756332 5.648631e-01 -0.0495739084  0.02706529
#> 7   -2.5959354 9.433379e-03 -0.0909299825 -0.01269318
#> 8   -2.7357278 6.224249e-03 -0.0911379667 -0.01505668
#> 9   -5.5326718 3.153893e-08 -0.1525711900 -0.07275040
#> 10   2.2030641 2.759023e-02  0.0036489891  0.06248803
#> 11   3.8461961 1.199658e-04  0.0283185283  0.08716950
#> 12   7.8846018 3.155402e-15  0.0897821271  0.14918483
#> 13   8.5684027 1.049303e-17  0.1147788432  0.18286222
#> 14  10.2546354 1.128009e-24  0.1456589551  0.21449503
#> 15  10.5121046 7.597876e-26  0.1432404450  0.20889561
#> 16  -3.2365512 1.209835e-03 -0.0417820481 -0.01026427
#> 17  -0.4028801 6.870364e-01 -0.0399674639  0.02633805
#> 18   0.8826121 3.774459e-01 -0.0181705721  0.04794277
#> 19   0.7775161 4.368543e-01 -0.0200310460  0.04637379
#> 20   2.1474227 3.175965e-02  0.0055811617  0.12228820
#> 21   2.2411389 2.501708e-02  0.0047454851  0.07090345
#> 22   4.3586841 1.308468e-05  0.0403324663  0.10624277
#> 23   2.7650788 5.690905e-03  0.0230320943  0.13517033
#> 24   5.1509344 2.591918e-07  0.0524939070  0.11697972
#> 25   4.4588289 8.240869e-06  0.0715314752  0.18374172
#> 26   5.4619014 4.710617e-08  0.1008555882  0.21374928
#> 27   5.9035395 3.557843e-09  0.0436140931  0.08696666
#> 28   9.2918621 1.516123e-20  0.0850754790  0.13056026
#> 29   9.9295561 3.096308e-23  0.0908142762  0.13548224
#> 30  10.6562033 1.631214e-26  0.1019519424  0.14790793
#> 31   2.1349955 3.276138e-02  0.0020673680  0.04836722
#> 32 -13.3471879 1.230053e-40 -0.1803999091 -0.13420225
#> 33  -4.9610045 7.012955e-07 -0.0785701821 -0.03406926
#> 34 -11.0744824 1.668414e-28 -0.1487226603 -0.10399639
#> 35 -13.7357127 6.204256e-43 -0.1825345244 -0.13694731
#> 36   4.4739991 7.677008e-06  0.0314422770  0.08046763
#> 37   4.2251645 2.387663e-05  0.0293517947  0.08014505
#> 38   5.9550477 2.599960e-09  0.0505287747  0.10010700
#> 39  -8.4212085 3.726204e-17 -0.1357054444 -0.08446310
#> 40  -4.2679592 1.972694e-05 -0.0559160957 -0.02072185
#> 41   3.2903404 1.000662e-03  0.0224945619  0.08877435
# }
```
