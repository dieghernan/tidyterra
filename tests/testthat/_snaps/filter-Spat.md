# filter validates .keep_extent

    Code
      filter(r, lyr > 1, .keep_extent = "yes")
    Condition
      Error in `filter()`:
      ! `.keep_extent` must be `TRUE` or `FALSE`, not the string "yes".

