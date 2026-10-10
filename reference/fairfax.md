# Shapefile of Fairfax County, Virginia, voting precincts

Boundaries of the 238 voting precincts in Fairfax County, Virginia, with
each precinct's ID, name, district, and polling place. One row per
precinct. The book uses it to build the adjacency matrix for the
experiment over networks; see
[`estimator_AS_tidy()`](https://declaredesign.org/r/rdss/reference/estimator_AS_tidy.md).

## Usage

``` r
fairfax
```

## Format

An sf object with 238 rows and 10 columns:

## Examples

``` r

names(fairfax)
#>  [1] "PREC_IDENT" "PREC_NAME"  "DISTRICT"   "POLLING_PL" "ADDRESS"   
#>  [6] "CITY"       "ZIP"        "SHAPE_AREA" "SHAPE_LEN"  "geometry"  
```
