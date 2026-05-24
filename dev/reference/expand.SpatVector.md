# Expand `SpatVector` attribute combinations

[`expand()`](https://tidyr.tidyverse.org/reference/expand.html) returns
a tibble with all combinations of selected attributes. It does not
return a `SpatVector` because newly created combinations do not have a
well-defined geometry. Use
[`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/complete.SpatVector.md)
when empty geometries should be added explicitly.

## Usage

``` r
# S3 method for class 'SpatVector'
expand(data, ..., .name_repair = "check_unique")
```

## Arguments

- data:

  A `SpatVector`.

- ...:

  \<[`data-masking`](https://tidyr.tidyverse.org/reference/tidyr_data_masking.html)\>
  Specification of columns to expand or complete. Columns can be atomic
  vectors or lists.

  - To find all unique combinations of `x`, `y` and `z`, including those
    not present in the data, supply each variable as a separate
    argument: `expand(df, x, y, z)` or `complete(df, x, y, z)`.

  - To find only the combinations that occur in the data, use `nesting`:
    `expand(df, nesting(x, y, z))`.

  - You can combine the two forms. For example,
    `expand(df, nesting(school_id, student_id), date)` would produce a
    row for each present school-student combination for all possible
    dates.

  When used with factors,
  [`expand()`](https://tidyr.tidyverse.org/reference/expand.html) and
  [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  use the full set of levels, not just those that appear in the data. If
  you want to use only the values seen in the data, use
  [`forcats::fct_drop()`](https://forcats.tidyverse.org/reference/fct_drop.html).

  When used with continuous variables, you may need to fill in values
  that do not appear in the data: to do so use expressions like
  `year = 2010:2020` or `year = full_seq(year,1)`.

- .name_repair:

  One of `"check_unique"`, `"unique"`, `"universal"`, `"minimal"`,
  `"unique_quiet"`, or `"universal_quiet"`. See
  [`vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Methods

Implementation of the **generic**
[`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html)
method.

### `SpatVector`

The output is a tibble with attribute combinations. Geometry is not
preserved because new combinations do not have a well-defined geometry.

## See also

[`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html),
[`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/complete.SpatVector.md)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) verbs for
handling missing values:
[`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/complete.SpatVector.md),
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/complete.SpatVector.md),
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md),
[`nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest.SpatVector.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md),
[`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md),
[`uncount.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/uncount.SpatVector.md),
[`unite.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/unite.Spat.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v$grp <- rep(c("A", "B"), length.out = nrow(v))

expand(v, grp, cpro)
#> # A tibble: 18 × 2
#>    grp   cpro 
#>    <chr> <chr>
#>  1 A     05   
#>  2 A     09   
#>  3 A     24   
#>  4 A     34   
#>  5 A     37   
#>  6 A     40   
#>  7 A     42   
#>  8 A     47   
#>  9 A     49   
#> 10 B     05   
#> 11 B     09   
#> 12 B     24   
#> 13 B     34   
#> 14 B     37   
#> 15 B     40   
#> 16 B     42   
#> 17 B     47   
#> 18 B     49   
```
