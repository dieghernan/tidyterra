test_that("With SpatRaster", {
  skip_on_cran()

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
    any(
      pull(r2, 1, xy = TRUE) == pull(r2, 1, xy = FALSE),
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
  skip_on_cran()

  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  df <- as_tibble(v)

  expect_identical(
    dplyr::pull(df, 1),
    pull(v, 1)
  )

  # Expect error if trying to get without opt
  expect_error(pull(v, geometry), "object 'geometry' not found")

  # With opts
  v_c <- terra::centroids(v)
  df_c <- as_tibble(v_c, geom = "XY")

  expect_identical(
    pull(v_c, x, geom = "XY"),
    dplyr::pull(df_c, x)
  )

  # Check with geometries Named
  wktgeom <- pull(v, geometry, iso2, geom = "WKT")

  expect_identical(names(wktgeom), pull(v, iso2))

  thegeom <- terra::geom(v, wkt = TRUE)
  un_wktgeom <- unname(wktgeom)
  expect_identical(un_wktgeom, thegeom)

  un_pull <- pull(v, geometry, geom = "WKT")
  expect_identical(un_pull, thegeom)

  # With hex
  hexgeom <- pull(v[1:2, ], geometry, geom = "HEX")
  thehexgeom <- terra::geom(v[1:2, ], hex = TRUE)
  expect_identical(hexgeom, thehexgeom)
})
