# grouping variables preserved with a msg, unless already selected

    Code
      res <- select(df, x)
    Message
      Adding missing grouping variables: `g`

---

    Code
      expect_equal(group_keys(select(df, a = c)), group_keys(group_by(tibble::tibble(
        b = 2, a = 3), b)))
    Message
      Adding missing grouping variables: `b`
    Code
      expect_equal(group_keys(select(df, b = c)), group_keys(group_by(tibble::tibble(
        a = 1, b = 3), a)))
    Message
      Adding missing grouping variables: `a`

