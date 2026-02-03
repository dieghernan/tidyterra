# Grouping metadata for `SpatVector` objects

This collection of functions accesses data about grouped `SpatVector`
objects in various ways:

- [`group_data()`](https://dplyr.tidyverse.org/reference/group_data.html)
  returns a tibble that defines the grouping structure. The columns give
  the values of the grouping variables. The last column, always called
  `.rows`, is a list of integer vectors that gives the location of the
  rows in each group.

- [`group_keys()`](https://dplyr.tidyverse.org/reference/group_data.html)
  returns a tibble describing the groups.

- [`group_rows()`](https://dplyr.tidyverse.org/reference/group_data.html)
  returns a list of integer vectors giving the rows that each group
  contains.

- [`group_indices()`](https://dplyr.tidyverse.org/reference/group_data.html)
  returns an integer vector the same length as `.data` that gives the
  group that each row belongs to.

- [`group_vars()`](https://dplyr.tidyverse.org/reference/group_data.html)
  gives names of grouping variables as character vector.

- [`groups()`](https://dplyr.tidyverse.org/reference/group_data.html)
  gives the names of the grouping variables as a list of symbols.

- [`group_size()`](https://dplyr.tidyverse.org/reference/group_data.html)
  gives the size of each group.

- [`n_groups()`](https://dplyr.tidyverse.org/reference/group_data.html)
  gives the total number of groups.

See
[`dplyr::group_data()`](https://dplyr.tidyverse.org/reference/group_data.html).

## Usage

``` r
# S3 method for class 'SpatVector'
group_data(.data)

# S3 method for class 'SpatVector'
group_keys(.tbl, ...)

# S3 method for class 'SpatVector'
group_indices(.data, ...)

# S3 method for class 'SpatVector'
group_vars(x)

# S3 method for class 'SpatVector'
groups(x)

# S3 method for class 'SpatVector'
group_size(x)

# S3 method for class 'SpatVector'
n_groups(x)
```

## Arguments

- .data, .tbl, x:

  A `SpatVector`.

- ...:

  Unused.

## Value

See the description of the method. The results are usually tibbles,
lists or vectors. These functions does not return `SpatVector` objects.

## Examples

``` r
library(terra)

v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v$gr_1 <- rep_len(c("A", "A", "B"), length.out = nrow(v))
v$gr_2 <- rep_len(c("C", "D"), length.out = nrow(v))

# Ungrouped

n_groups(v)
#> [1] 1

group_vars(v)
#> character(0)

group_keys(v)
#> # A tibble: 1 × 0

group_size(v)
#> [1] 9

groups(v)
#> list()

group_rows(v)
#> <list_of<integer>[1]>
#> [[1]]
#> [1] 1 2 3 4 5 6 7 8 9
#> 

group_data(v)
#> # A tibble: 1 × 1
#>         .rows
#>   <list<int>>
#> 1         [9]

group_indices(v)
#> [1] 1 1 1 1 1 1 1 1 1

# Grouped by one var
gv <- group_by(v, gr_1)

n_groups(gv)
#> [1] 2

group_vars(gv)
#> [1] "gr_1"

group_keys(gv)
#> # A tibble: 2 × 1
#>   gr_1 
#>   <chr>
#> 1 A    
#> 2 B    

group_size(gv)
#> [1] 6 3

groups(gv)
#> [[1]]
#> gr_1
#> 

group_rows(gv)
#> <list_of<integer>[2]>
#> [[1]]
#> [1] 1 2 4 5 7 8
#> 
#> [[2]]
#> [1] 3 6 9
#> 

group_data(gv)
#> # A tibble: 2 × 2
#>   gr_1        .rows
#>   <chr> <list<int>>
#> 1 A             [6]
#> 2 B             [3]

group_indices(gv)
#> [1] 1 1 2 1 1 2 1 1 2

# Grouped by several vars

gv2 <- group_by(v, gr_1, gr_2)

n_groups(gv2)
#> [1] 4

group_vars(gv2)
#> [1] "gr_1" "gr_2"

group_keys(gv2)
#> # A tibble: 4 × 2
#>   gr_1  gr_2 
#>   <chr> <chr>
#> 1 A     C    
#> 2 A     D    
#> 3 B     C    
#> 4 B     D    

group_size(gv2)
#> [1] 3 3 2 1

groups(gv2)
#> [[1]]
#> gr_1
#> 
#> [[2]]
#> gr_2
#> 

group_rows(gv2)
#> <list_of<integer>[4]>
#> [[1]]
#> [1] 1 5 7
#> 
#> [[2]]
#> [1] 2 4 8
#> 
#> [[3]]
#> [1] 3 9
#> 
#> [[4]]
#> [1] 6
#> 

group_data(gv2)
#> # A tibble: 4 × 3
#>   gr_1  gr_2        .rows
#>   <chr> <chr> <list<int>>
#> 1 A     C             [3]
#> 2 A     D             [3]
#> 3 B     C             [2]
#> 4 B     D             [1]

group_indices(gv2)
#> [1] 1 2 3 2 1 4 1 2 3
```
