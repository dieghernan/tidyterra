# Check if x and y positions conforms a regular grid

Assess if the coordinates x,y of an object conforms a regular grid. This
function is called by its side effects.

This function is internally called by
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatraster.md).

## Usage

``` r
is_regular_grid(xy, digits = 6)
```

## Arguments

- xy:

  A matrix, data frame or tibble of at least two columns representing x
  and y coordinates.

- digits:

  integer to set the precision for detecting whether points are on a
  regular grid (a low number of digits is a low precision).

## Value

[`invisible()`](https://rdrr.io/r/base/invisible.html) if is regular or
an error message otherwise

## See also

[`as_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatraster.md)

Other helpers:
[`compare_spatrasters()`](https://dieghernan.github.io/tidyterra/dev/reference/compare_spatrasters.md),
[`is_grouped_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/is_grouped_spatvector.md),
[`pull_crs()`](https://dieghernan.github.io/tidyterra/dev/reference/pull_crs.md)

## Examples

``` r
p <- matrix(1:90, nrow = 45, ncol = 2)

is_regular_grid(p)


# Jitter location
set.seed(1234)
jitter <- runif(length(p)) / 10e4
p_jitter <- p + jitter

# Need to adjust digits
is_regular_grid(p_jitter, digits = 4)
```
