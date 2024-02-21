# handles duplicated column names

    Code
      pv <- pivot_longer(df, -x)
    Message
      New names:
      * `a` -> `a...2`
      * `a` -> `a...3`
      * `b` -> `b...4`
      * `b` -> `b...5`
    Condition
      Error in `pivot_longer()`:
      ! Can't pivot "geometry" of the <SpatVector> with these args

