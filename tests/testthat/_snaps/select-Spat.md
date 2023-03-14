# grouping variables preserved with a msg, unless already selected

    Code
      res <- select(df, x)
    Message <rlang_message>
      Adding missing grouping variables: `g`

---

    Code
      expect_equal(df %>% select(a = c) %>% group_keys(), tibble::tibble(b = 2, a = 3) %>%
        group_by(b) %>% group_keys())
    Message <rlang_message>
      Adding missing grouping variables: `b`
    Code
      expect_equal(df %>% select(b = c) %>% group_keys(), tibble::tibble(a = 1, b = 3) %>%
        group_by(a) %>% group_keys())
    Message <rlang_message>
      Adding missing grouping variables: `a`

