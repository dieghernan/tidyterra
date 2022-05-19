test_that("With SpatRaster", {
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)
  df <- as_tibble(r)


  expect_identical(
    dplyr::pull(df, 1),
    pull(r, 1)
  )

  expect_identical(
    dplyr::pull(df, -1),
    pull(r, -1)
  )

  # Cut
  r2 <- r %>% mutate(is_fact = cut(tavg_04, seq(0, 20, 5)))

  # With na.rm FALSE should have more lenght
  expect_lt(
    length(pull(r2, na.rm = TRUE)),
    length(pull(r2, na.rm = FALSE))
  )

  expect_false(anyNA(pull(r2, na.rm = TRUE)))
  expect_true(anyNA(pull(r2, na.rm = FALSE)))


  df2 <- as_tibble(r2)

  expect_true(is.factor(pull(r2, "is_fact")))
  expect_true(is.factor(pull(r2, is_fact)))
  expect_identical(
    dplyr::pull(df2, is_fact),
    pull(r2, is_fact)
  )


  # This should change

  expect_false(
    any(pull(r2, 1, xy = TRUE) ==
      pull(r2, 1, xy = FALSE),
    na.rm = TRUE
    )
  )

  # This would error

  expect_error(pull(r2, y, xy = FALSE))

  # This not
  expect_silent(pull(r2, y, xy = TRUE))

  # Named
  a <- pull(r2, 1, name = is_fact)
  b <- pull(r2, 1)

  expect_true(all(a == b, na.rm = TRUE))

  expect_true(length(unique(names(a))) > 0)
  expect_true(is.null(unique(names(b))))
})


test_that("With SpatVector", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  df <- as_tibble(v)

  expect_identical(
    dplyr::pull(df, 1),
    pull(v, 1)
  )

  # With opts
  v_c <- terra::centroids(v)
  df_c <- as_tibble(v_c, geom = "XY")

  expect_identical(
    pull(v_c, x, geom = "XY"),
    dplyr::pull(df_c, x)
  )
})
