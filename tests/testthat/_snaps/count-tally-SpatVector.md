# name must be string

    Code
      count(df1, x, name = 1)
    Error <rlang_error>
      `name` must be a single string, not the number 1.

---

    Code
      count(df1, x, name = letters)
    Error <rlang_error>
      `name` must be a single string, not a character vector.

# can only explicitly chain together multiple tallies

    Code
      df <- data.frame(g = c(1, 1, 2, 2), n = 1:4)
      df$lat <- 1:4
      df$lon <- 1:4
      df <- terra::vect(df, crs = "EPSG:3857")
      df %>% count(g)
    Output
       class       : SpatVector 
       geometry    : points 
       dimensions  : 2, 2  (geometries, attributes)
       extent      : 1, 4, 1, 4  (xmin, xmax, ymin, ymax)
       coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
       names       :     g     n
       type        : <num> <int>
       values      :     1     2
                         2     2
    Code
      df %>% count(g) %>% count()
    Output
       class       : SpatVector 
       geometry    : points 
       dimensions  : 1, 1  (geometries, attributes)
       extent      : 1, 4, 1, 4  (xmin, xmax, ymin, ymax)
       coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
       names       :     n
       type        : <int>
       values      :     2
    Code
      df %>% count(n)
    Message <rlang_message>
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

