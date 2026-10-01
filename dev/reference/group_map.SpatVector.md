# Apply a function to each [`SpatVector`](https://rspatial.github.io/terra/reference/vect.html) group

**\[experimental\]**

[`dplyr::group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
and
[`dplyr::group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html)
are functions in the style of
[purrr](https://CRAN.R-project.org/package=purrr) that can be used to
iterate on grouped
[`SpatVector`](https://rspatial.github.io/terra/reference/vect.html)
objects.

## Usage

``` r
# S3 method for class 'SpatVector'
group_map(.data, .f, ..., .keep = FALSE)

# S3 method for class 'SpatVector'
group_modify(.data, .f, ..., .keep = FALSE)
```

## Arguments

- .data:

  A grouped or ungrouped `SpatVector`.

- .f:

  A function called with `.x`, a `SpatVector` containing the rows for
  one group, and `.y`, a
  [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  with the group keys.

- ...:

  Additional arguments passed on to `.f`

- .keep:

  are the grouping variables kept in `.x`

## Value

[`group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
returns a list of results from calling `.f` on each group.
[`group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html)
returns a
[`SpatVector`](https://rspatial.github.io/terra/reference/vect.html)
object and requires `.f` to return
[`SpatVector`](https://rspatial.github.io/terra/reference/vect.html)
objects.

## Details

Each conceptual group is exposed to `.f` with two pieces of information:
`.x`, the subset of rows for the group as a
[`SpatVector`](https://rspatial.github.io/terra/reference/vect.html),
and `.y`, a one-row
[tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html) with
one column per grouping variable that identifies the group.

These methods also work on ungrouped
[`SpatVector`](https://rspatial.github.io/terra/reference/vect.html)
objects. In that case, `.f` is applied to the entire object and `.y` is
a one-row
[tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html) with
no columns.

## Methods

Implementation of the **generic**
[`dplyr::group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
method family for
[`SpatVector`](https://rspatial.github.io/terra/reference/vect.html)
objects.

[`group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
applies `.f` to each group and returns a list.
[`group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html)
requires `.f` to return
[`SpatVector`](https://rspatial.github.io/terra/reference/vect.html)
objects and binds the results.

## See also

[`dplyr::group_map()`](https://dplyr.tidyverse.org/reference/group_map.html),
[`dplyr::group_modify()`](https://dplyr.tidyverse.org/reference/group_map.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) grouping
methods:
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md),
[`group_data.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_data.SpatVector.md),
[`group_nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_nest.SpatVector.md),
[`group_split.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_split.SpatVector.md),
[`group_trim.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_trim.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v$grp <- rep(c("A", "B"), length.out = nrow(v))

group_map(group_by(v, grp), ~ nrow(.x))
#> [[1]]
#> [1] 5
#> 
#> [[2]]
#> [1] 4
#> 

group_modify(group_by(v, grp), ~ mutate(.x, key = .y$grp))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 4  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro      name   key
#> type        : <chr> <chr>     <chr> <chr>
#> values      : ES-AV    05     Avila     A
#>               ES-LE    24      Leon     A
#>               ES-SA    37 Salamanca     A
#>               ...
```
