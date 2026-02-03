# name must be string

    Code
      count(df1, x, name = 1)
    Condition
      Error in `dplyr::tally()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      count(df1, x, name = letters)
    Condition
      Error in `dplyr::tally()`:
      ! `name` must be a single string, not a character vector.

# .drop argument deprecated

    Code
      res <- count(df, f, .drop = FALSE)
    Condition
      Warning:
      The `.drop` argument of `count.SpatVector()` is deprecated as of tidyterra 1.1.0.
      i Argument not longer supported; empty groups are always removed(see `dplyr::count()`, `.drop = TRUE` argument).

# can only explicitly chain together multiple tallies

    Code
      df <- data.frame(g = c(1, 1, 2, 2), n = 1:4)
      df$lat <- 1:4
      df$lon <- 1:4
      df <- terra::vect(df, crs = "EPSG:3857")
      count(df, g, wt = n)
    Output
       class       : SpatVector 
       geometry    : points 
       dimensions  : 2, 2  (geometries, attributes)
       extent      : 1, 4, 1, 4  (xmin, xmax, ymin, ymax)
       coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
       names       :     g     n
       type        : <num> <int>
       values      :     1     3
                         2     7
    Code
      count(count(df, g, wt = n), wt = n)
    Output
       class       : SpatVector 
       geometry    : points 
       dimensions  : 1, 1  (geometries, attributes)
       extent      : 1, 4, 1, 4  (xmin, xmax, ymin, ymax)
       coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
       names       :     n
       type        : <int>
       values      :    10
    Code
      count(df, n)
    Message
      Storing counts in `nn`, as `n` already present in input
      i Use `name = "new_name"` to pick a new name.
      Storing counts in `nn`, as `n` already present in input
      i Use `name = "new_name"` to pick a new name.
    Output
       class       : SpatVector 
       geometry    : points 
       dimensions  : 4, 2  (geometries, attributes)
       extent      : 1, 4, 1, 4  (xmin, xmax, ymin, ymax)
       coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
       names       :     n    nn
       type        : <int> <int>
       values      :     1     1
                         2     1
                         3     1

