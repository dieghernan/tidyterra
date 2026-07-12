# autoplot validates tri-state logical arguments

    Code
      autoplot(r, rgb = "auto")
    Condition
      Error in `autoplot()`:
      ! `rgb` must be `TRUE`, `FALSE`, or `NULL`, not the string "auto".

---

    Code
      autoplot(r, use_coltab = "auto")
    Condition
      Error in `autoplot()`:
      ! `use_coltab` must be `TRUE`, `FALSE`, or `NULL`, not the string "auto".

---

    Code
      autoplot(r, facets = "auto")
    Condition
      Error in `autoplot()`:
      ! `facets` must be `TRUE`, `FALSE`, or `NULL`, not the string "auto".

