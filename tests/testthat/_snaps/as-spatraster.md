# Error check

    Code
      as_spatraster(as_tbl, xycols = 2)
    Condition
      Error in `as_spatraster()`:
      ! `xycols` must be a numeric vector of 2 whole numbers, not <numeric>.

---

    Code
      as_spatraster(as_tbl, xycols = c("x", "y"))
    Condition
      Error in `as_spatraster()`:
      ! `xycols` must be a numeric vector of 2 whole numbers, not <character>.

---

    Code
      as_spatraster(as_tbl, xycols = 1:3)
    Condition
      Error in `as_spatraster()`:
      ! `xycols` must be a numeric vector of 2 whole numbers, not <integer>.

---

    Code
      as_spatraster(as.matrix(as_tbl))
    Condition
      Error in `as_spatraster()`:
      ! `x` must be a data frame, not a double matrix.

# Regenerate raster properly

    Code
      res <- compare_spatrasters(r, fromnonatr)
    Message
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * CRS

# Irregular grids

    Code
      as_spatraster(p_jitter_x)
    Condition
      Error in `is_regular_grid()`:
      ! `x` cell sizes are not regular. Try with a lower `digits` value.

---

    Code
      as_spatraster(p_jitter_y)
    Condition
      Error in `is_regular_grid()`:
      ! `y` cell sizes are not regular. Try with a lower `digits` value.

# Check internal

    Code
      as_spat_internal(tbl2)
    Condition
      Error in `as_spat_internal()`:
      ! Cannot convert `x` back to a <Spat*> object. Required reconstruction attributes are missing.

