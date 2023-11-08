# Error check

    Code
      as_spatvector(as_tbl)
    Condition
      Error in `as_spatvector()`:
      ! Columns `lon` and `lat` not found in `x`

---

    Code
      as_spatvector(as_tbl, geom = NA)
    Condition
      Error in `as_spatvector()`:
      ! `geom` should be a <character>, not <logical>

---

    Code
      as_spatvector(as_tbl, geom = c("a", "b", "c"))
    Condition
      Error in `as_spatvector()`:
      ! `geom` should be of length 1 or 2, not 3

---

    Code
      as_spatvector(as_tbl, geom = 1)
    Condition
      Error in `as_spatvector()`:
      ! `geom` should be a <character>, not <numeric>

