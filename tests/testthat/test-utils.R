test_that("make_safe_index works", {
  df <- data.frame(a = 1, b = 1, c = 1)
  expect_equal("tterra_index", make_safe_index("tterra_index", df))
  expect_equal("aname", make_safe_index("aname", df))
  expect_equal("a_001", make_safe_index("a", df))

  names(df) <- c("a_002", "b", "a")
  expect_equal("a_001", make_safe_index("a", df))

  names(df) <- c("a_002", "a", "a_001")
  expect_equal("a_003", make_safe_index("a", df))
})

test_that("make_safe_index iterates until make safe name", {
  m <- matrix(nrow = 1, ncol = 20)
  df <- as.data.frame(m)


  nnames <- paste0("a_", sprintf("%03d", seq_len(ncol(df))))
  names(df) <- nnames
  expect_identical(names(df), nnames)

  # Add a
  df_new <- cbind(df, data.frame(a = 1))

  expect_equal("a_021", make_safe_index("a", df_new))
})
