# Pipe operator

See `magrittr::%>%` for details.

## Usage

``` r
lhs %>% rhs
```

## Arguments

- lhs:

  A value or the [magrittr](https://CRAN.R-project.org/package=magrittr)
  placeholder.

- rhs:

  A function call using
  [magrittr](https://CRAN.R-project.org/package=magrittr) semantics.

## Value

The result of calling `rhs(lhs)`.
