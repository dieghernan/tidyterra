test_that("Drop na with SpatVectors", {
  skip_on_cran()

  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)
  v[1, 1] <- NA

  dropped1 <- drop_na(v)

  # Check with tibble method
  dropped_check <- tidyr::drop_na(as_tibble(v))

  expect_s4_class(dropped1, "SpatVector")
  expect_length(dropped1, nrow(v) - 1)

  expect_identical(as_tibble(dropped1), dropped_check)

  nona <- drop_na(v, cpro)
  nonacheck <- tidyr::drop_na(as_tibble(v), cpro)
  expect_s4_class(nona, "SpatVector")
  expect_length(nona, nrow(v))

  expect_identical(as_tibble(nona), nonacheck)
})


test_that("groups are preserved", {
  skip_on_cran()

  df <- data.frame(g = c("A", "A", "B"), x = c(1, 2, NA), y = c("a", NA, "b"))
  df$lon <- seq_len(nrow(df))
  df$lat <- df$lon
  df <- terra::vect(df)
  exp <- data.frame(g = c("A", "B"), x = c(1, NA), y = c("a", "b"))

  gdf <- group_by(df, g)

  gexp <- dplyr::group_by(exp, g)

  res <- drop_na(gdf, y)
  expect_s4_class(res, "SpatVector")

  expect_identical(as.data.frame(res), as.data.frame(gexp))
  expect_identical(group_vars(res), dplyr::group_vars(gexp))
})

test_that("Return empty geom when no results", {
  skip_on_cran()

  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)
  v$cpro <- NA
  expect_snapshot(empty <- drop_na(v))

  expect_equal(ncol(empty), 0)
  expect_equal(nrow(empty), 1)

  tbl2 <- tibble::tibble(geometry = "POINT EMPTY")
  attr(tbl2, "crs") <- pull_crs(v)
  expect_identical(as_tibble(empty, geom = "WKT"), tbl2)

  # Does not preserve crs if not provided
  nocrs <- v
  terra::crs(nocrs) <- NA
  expect_snapshot(emptycrs <- drop_na(nocrs))

  expect_identical(
    as.data.frame(emptycrs, geom = "WKT"),
    data.frame(geometry = "POINT EMPTY")
  )
  expect_true(is.na(pull_crs(emptycrs)))
})

test_that("Drop na with SpatRaster", {
  skip_on_cran()

  r <- terra::rast(
    extent = c(0, 10, 0, 10),
    nlyr = 3,
    resolution = c(2.5, 2.5)
  )

  terra::values(r) <- seq_len(terra::ncell(r) * terra::nlyr(r))
  # Add NAs
  r[r > 31 & r < 45] <- NA

  # Extract as tibble for comparison
  tbl <- as_tibble(r, na.rm = FALSE)

  # Drop all
  all <- drop_na(r)



  expect_snapshot(res <- compare_spatrasters(r, all))
  expect_false(res)
  expect_equal(
    nrow(tidyr::drop_na(tbl)),
    terra::ncell(all)
  )


  # Drop based on layer
  lyr <- drop_na(r, lyr.1)

  expect_silent(compare_spatrasters(r, lyr))
  expect_true(compare_spatrasters(r, lyr))
  expect_equal(
    nrow(tidyr::drop_na(tbl, lyr.1)),
    terra::ncell(lyr)
  )
})
