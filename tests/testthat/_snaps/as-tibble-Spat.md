# For SpatVector

    Code
      res <- dplyr::as_tibble(v2, geom = "WKT")
    Message
      i Column(s) with duplicated/reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns:
    Output
      * `geometry` -> `geometry.1`

---

    Code
      res <- dplyr::as_tibble(v2, geom = "HEX")
    Message
      i Column(s) with duplicated/reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns:
    Output
      * `geometry` -> `geometry.1`

---

    Code
      res_p <- dplyr::as_tibble(p, geom = "XY")
    Message
      i Column(s) with duplicated/reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns:
    Output
      * `x` -> `x.1`
      * `y` -> `y.1`

# For SpatVector internal

    Code
      as_tbl_internal(ntibble)
    Condition
      Error in `as_tbl_internal()`:
      ! `x` is not of <SpatRaster> or <SpatVector> object

