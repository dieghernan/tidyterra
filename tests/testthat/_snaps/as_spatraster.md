# Error check

    Code
      as_spatraster(as_tbl, xycols = 2)
    Condition
      Error in `as_spatraster()`:
      ! `xycols` should have a length of 2, not 1

---

    Code
      as_spatraster(as_tbl, xycols = c("x", "y"))
    Condition
      Error in `as_spatraster()`:
      ! `xycols` should be a <integer>, not <character>

---

    Code
      as_spatraster(as_tbl, xycols = 1:3)
    Condition
      Error in `as_spatraster()`:
      ! `xycols` should have a length of 2, not 3

---

    Code
      as_spatraster(as.matrix(as_tbl))
    Condition
      Error in `as_spatraster()`:
      ! `x` should be a <data.frame/tbl>, not <matrix/array>

# Regenerate raster properly

    Code
      res <- compare_spatrasters(r, fromnonatr)
    Message
      ! Results of `tidyterra::compare_spatrasters()`: 
      The following attributes are not equal:
      * crs

