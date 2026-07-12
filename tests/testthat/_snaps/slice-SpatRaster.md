# slice validates .keep_extent

    Code
      slice(r, 1:4, .keep_extent = "yes")
    Condition
      Error in `slice()`:
      ! `.keep_extent` must be `TRUE` or `FALSE`, not the string "yes".

# slice_colrows validates inverse

    Code
      slice_colrows(r, cols = 2, rows = 2, inverse = "yes")
    Condition
      Error in `slice_colrows()`:
      ! `inverse` must be `TRUE` or `FALSE`, not the string "yes".

