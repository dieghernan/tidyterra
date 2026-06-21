# A grouped `SpatVector`

The easiest way to create a grouped `SpatVector` is to call the
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
method on a `SpatVector`: this will take care of capturing the
unevaluated expressions for you. See
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md)
for details.

This function is an adapted version of
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

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

## Value

`TRUE` if `x` is a grouped `SpatVector`, otherwise `FALSE`.

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

is_grouped_spatvector(v)
#> [1] FALSE

grouped <- group_by(v, iso2)

is_grouped_spatvector(grouped)
#> [1] TRUE
```
