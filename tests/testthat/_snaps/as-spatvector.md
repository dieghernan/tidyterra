# Error check

    Code
      as_spatvector(as_tbl)
    Condition
      Error in `as_spatvector()`:
      ! Columns `lon` and `lat` are not found in `x`.

---

    Code
      as_spatvector(as_tbl, geom = NA)
    Condition
      Error in `as_spatvector()`:
      ! `geom` must be a <character> vector, not <logical>.

---

    Code
      as_spatvector(as_tbl, geom = c("a", "b", "c"))
    Condition
      Error in `as_spatvector()`:
      ! `geom` must have length 1 or 2, not 3.

---

    Code
      as_spatvector(as_tbl, geom = 1)
    Condition
      Error in `as_spatvector()`:
      ! `geom` must be a <character> vector, not <numeric>.

