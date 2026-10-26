# Teaching data based on the 2018/19 LAPOP AmericasBarometer survey of Brazil

Used in the book's student exercises. These data were resampled with
replacement from the LAPOP data, to 10,000 rows, for a subset of
variables. These data cannot be used for scientific inferences, and are
only useful for teaching purposes. ID numbers were scrambled so that
individuals and municipalities cannot easily be identified.

## Usage

``` r
lapop_brazil
```

## Format

A data frame with 10,000 rows and 10 columns: `ID`, `municipality`, and
eight survey items (`trust_police`, `govt_responsive`, `ideology`,
`govt_pride`, `self_efficacy_political`, `trust_military`,
`trust_supreme_court`, and `support_political_system`).

## Source

LAPOP AmericasBarometer 2018/19 survey of Brazil (fieldwork January to
March 2019), Vanderbilt University,
<https://www.vanderbilt.edu/cgd/data-access/>.

## Details

Download the original data from
https://www.vanderbilt.edu/cgd/data-access/

See https://www.vanderbilt.edu/lapop/core-surveys.php for the survey
questionnaire.

## Examples

``` r

summary(lapop_brazil$trust_police)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.000   3.000   5.000   4.427   6.000   7.000 
```
