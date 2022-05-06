test_that("Check crs", {
  expect_message(pull_crs(list(1)))
  expect_true(is.na(pull_crs(list(1))))

  expect_true(is.na(pull_crs(NA)))
  expect_silent(is.na(pull_crs(NA)))

  expect_true(is.na(pull_crs(NULL)))
  expect_silent(is.na(pull_crs(NULL)))

  expect_true(is.na(pull_crs("")))
  expect_silent(is.na(pull_crs("")))

  expect_error(pull_crs("Some string"))

  # Base for comparison
  base <- pull_crs(4326)

  sfobj <- sf::st_as_sfc("MULTIPOINT ((0 0), (1 1))", crs = 4326)

  expect_s3_class(sfobj, "sfc")
  sfc <- pull_crs(sfobj)

  sfobj2 <- sf::st_sf(sfobj)

  expect_s3_class(sfobj2, "sf")
  sf2 <- pull_crs(sfobj2)

  crs <- sf::st_crs(sfobj)

  expect_s3_class(crs, "crs")
  crs2 <- pull_crs(crs)

  # terra

  v <- terra::vect(sfobj2)

  expect_s4_class(v, "SpatVector")
  v2 <- pull_crs(v)

  r <- terra::rast(v)

  expect_s4_class(r, "SpatRaster")
  r2 <- pull_crs(r)

  # Characters
  fromchar <- pull_crs("epsg:4326")

  expect_true(all.equal(
    base,
    sfc,
    sf2,
    crs2,
    r2,
    v2,
    fromchar
  ))
})
