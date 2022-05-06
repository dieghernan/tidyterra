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
