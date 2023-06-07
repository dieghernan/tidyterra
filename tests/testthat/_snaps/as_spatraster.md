# Error check

    Code
      as_spatraster(as_tbl, xycols = 2)
    Error <rlang_error>
      `xycols` should have a length of 2, not 1

---

    Code
      as_spatraster(as_tbl, xycols = c("x", "y"))
    Error <rlang_error>
      `xycols` should be a <integer>, not <character>

---

    Code
      as_spatraster(as_tbl, xycols = 1:3)
    Error <rlang_error>
      `xycols` should have a length of 2, not 3

---

    Code
      as_spatraster(as.matrix(as_tbl))
    Error <rlang_error>
      `x` should be a <data.frame/tbl>, not <matrix/array>

# Regenerate raster properly

    Code
      res <- compare_spatrasters(r, fromnonatr)
    Message <cliMessage>
      ! Results of compare_spatrasters() (tidyterra) 
      The following attributes are not equal:
         - crs 

