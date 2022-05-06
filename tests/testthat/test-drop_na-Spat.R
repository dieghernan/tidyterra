test_that("Drop na with SpatVectors", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)
  v[1, 1] <- NA

  dropped1 <- drop_na(v)

  expect_s4_class(dropped1, "SpatVector")
  expect_length(dropped1, nrow(v) - 1)

  nona <- drop_na(v, cpro)
  expect_s4_class(nona, "SpatVector")
  expect_length(nona, nrow(v))
})

test_that("Drop na with SpatRaster", {
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



  expect_message(compare_spatrasters(r, all))
  expect_false(compare_spatrasters(r, all))
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
