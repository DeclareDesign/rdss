# Tidy helper function for did_multiplegt

Runs
[`DIDmultiplegt::did_multiplegt()`](https://rdrr.io/pkg/DIDmultiplegt/man/did_multiplegt.html),
the de Chaisemartin and D'Haultfoeuille estimator, and returns its
estimate as a one-row data frame.

## Usage

``` r
did_multiplegt_tidy(data, ...)
```

## Arguments

- data:

  A data frame, passed to `did_multiplegt()` as `df`.

- ...:

  Further arguments to `did_multiplegt()`, which must include the column
  names of the outcome (`Y`), group (`G`), time period (`T`), and
  treatment (`D`), as strings.

## Value

A data frame with one column, `estimate`.

## Details

See
https://book.declaredesign.org/observational-causal.html#difference-in-differences
