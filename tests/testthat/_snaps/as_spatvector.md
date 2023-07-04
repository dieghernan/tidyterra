# Error check

    Code
      as_spatvector(as_tbl)
    Error <rlang_error>
      Columns `lon` and `lat` not found in `x`

---

    Code
      as_spatvector(as_tbl, geom = NA)
    Error <rlang_error>
      `geom` should be a <character>, not <logical>

---

    Code
      as_spatvector(as_tbl, geom = c("a", "b", "c"))
    Error <rlang_error>
      `geom` should be of length 1 or 2, not 3

---

    Code
      as_spatvector(as_tbl, geom = 1)
    Error <rlang_error>
      `geom` should be a <character>, not <numeric>

