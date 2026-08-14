# Check crs

    Code
      res <- pull_crs(list(1))
    Message
      ! `tidyterra::pull_crs()` could not find a WKT equivalent. Returning NA.

---

    Code
      pull_crs("Some string")
    Condition
      Error in `st_crs.character()`:
      ! invalid crs: Some string

