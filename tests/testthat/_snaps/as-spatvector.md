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
      ! `geom` must be a character vector, not <logical>.

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
      ! `geom` must be a character vector, not <numeric>.

---

    Code
      as_spatvector(as.matrix(as_tbl))
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'as_spatvector' applied to an object of class "c('matrix', 'array', 'integer', 'numeric')"

# Check internal

    Code
      as_spat_internal(tbl2)
    Condition
      Error in `as_spat_internal()`:
      ! Cannot convert `x` back to a <Spat*> object. Required reconstruction attributes are missing.

