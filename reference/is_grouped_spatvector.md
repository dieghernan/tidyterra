# A grouped `SpatVector`

The easiest way to create a grouped `SpatVector` is to call the
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
method on a `SpatVector`: this will take care of capturing the
unevaluated expressions for you. See
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md)
for details.

This function is the adapted version of
[`dplyr::is_grouped_df()`](https://dplyr.tidyverse.org/reference/grouped_df.html).

See also
[`group_data.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_data.SpatVector.md)
for the accessory functions that retrieve various metadata from a
grouped `SpatVector`.

## Usage

``` r
is_grouped_spatvector(x)
```

## Arguments

- x:

  a `SpatVector`.

## See also

Other helpers:
[`compare_spatrasters()`](https://dieghernan.github.io/tidyterra/reference/compare_spatrasters.md),
[`is_regular_grid()`](https://dieghernan.github.io/tidyterra/reference/is_regular_grid.md),
[`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)
