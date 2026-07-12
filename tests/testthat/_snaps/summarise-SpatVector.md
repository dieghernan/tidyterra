# summarise validates .dissolve

    Code
      summarise(v, sum_all = sum(AREA), .dissolve = "yes")
    Condition
      Error in `summarise()`:
      ! `.dissolve` must be `TRUE` or `FALSE`, not the string "yes".

