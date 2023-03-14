test_that("SpatRaster select: Geographic", {
  file <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  raster <- terra::rast(file)

  rasterselect <- raster %>% select(cyl_tile_1, cyl_tile_2)


  expect_true(terra::nlyr(rasterselect) == 2)

  # Checks on raster
  expect_s4_class(rasterselect, "SpatRaster")

  expect_true(compare_spatrasters(raster, rasterselect))
  expect_silent(compare_spatrasters(raster, rasterselect))
  expect_equal(terra::ncell(raster), terra::ncell(rasterselect))
})

test_that("SpatRaster select and rename: Geographic", {
  file <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  raster <- terra::rast(file)

  rasterselect <- raster %>% dplyr::select(name_test = cyl_tile_1)


  expect_true(terra::nlyr(rasterselect) == 1)
  expect_true(names(rasterselect) == "name_test")

  # Checks on raster
  expect_s4_class(rasterselect, "SpatRaster")

  expect_true(compare_spatrasters(raster, rasterselect))
  expect_silent(compare_spatrasters(raster, rasterselect))
  expect_equal(terra::ncell(raster), terra::ncell(rasterselect))
})

test_that("SpatRaster select: Non Geographic", {
  raster <- terra::rast(crs = NA, extent = c(0, 100, 0, 100), nlyr = 2)
  values <- seq_len(terra::ncell(raster) * terra::nlyr(raster))
  terra::values(raster) <- values

  rasterselect <- raster %>% dplyr::select(lyr.1)

  expect_true(terra::nlyr(rasterselect) == 1)
  expect_true(names(rasterselect) == "lyr.1")

  # Checks on raster
  expect_s4_class(rasterselect, "SpatRaster")

  expect_true(compare_spatrasters(raster, rasterselect))
  expect_silent(compare_spatrasters(raster, rasterselect))
  expect_equal(terra::ncell(raster), terra::ncell(rasterselect))
})


test_that("SpatVector select and rename", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  selected <- v %>% select(iso2, cpro2 = cpro)


  expect_s4_class(selected, "SpatVector")
  expect_equal(ncol(selected), 2)
  expect_equal(nrow(selected), nrow(v))
})


test_that("select preserves grouping", {
  df <- data.frame(g = 1:3, x = 3:1)
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)
  expect_s4_class(df, "SpatVector")

  gf <- group_by(df, g)

  out <- select(gf, h = g)
  expect_equal(group_vars(out), "h")
})

test_that("grouping variables preserved with a msg, unless already selected", {
  df <- data.frame(g = 1:3, x = 3:1)
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)
  df <- group_by(df, g)
  expect_s4_class(df, "SpatVector")

  expect_snapshot({
    res <- select(df, x)
  })
  expect_identical(names(res), c("g", "x"))

  df <- data.frame(a = 1, b = 2, c = 3)
  df <- terra::vect(df, geom = c("a", "b"), keepgeom = TRUE)
  df <- df %>% group_by(a)

  result <- df %>%
    select(a = b) %>%
    as_tibble()
  attr(result, "crs") <- NULL
  expect_equal(
    result,
    tibble::tibble(a = 2)
  )

  df <- data.frame(a = 1, b = 2, c = 3) %>%
    terra::vect(geom = c("a", "b"), keepgeom = TRUE) %>%
    group_by(a, b)

  expect_snapshot({
    expect_equal(
      df %>% select(a = c) %>% group_keys(),
      tibble::tibble(b = 2, a = 3) %>%
        group_by(b) %>%
        group_keys()
    )
    expect_equal(
      df %>% select(b = c) %>% group_keys(),
      tibble::tibble(a = 1, b = 3) %>%
        group_by(a) %>%
        group_keys()
    )
  })
})
