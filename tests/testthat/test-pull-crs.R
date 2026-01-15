test_that("Check crs", {
  expect_snapshot(res <- pull_crs(list(1)))
  expect_true(is.na(res))

  expect_true(is.na(pull_crs(NA)))
  expect_silent(is.na(pull_crs(NA)))

  expect_true(is.na(pull_crs(NULL)))
  expect_silent(is.na(pull_crs(NULL)))

  expect_true(is.na(pull_crs("")))
  expect_silent(is.na(pull_crs("")))

  expect_error(pull_crs("Some string"))
  skip_on_cran()

  # Base for comparison
  base <- pull_crs(4326)

  sfobj <- sf::st_as_sfc("MULTIPOINT ((0 0), (1 1))", crs = 4326)

  expect_s3_class(sfobj, "sfc")
  sfc <- pull_crs(sfobj)

  sfobj2 <- sf::st_sf(sfobj)

  expect_s3_class(sfobj2, "sf")
  sf2 <- pull_crs(sfobj2)

  # On sf with columns with NA also works
  sf_na <- sfobj2

  sf_na$no_na <- "A"
  sf_na$no_na <- NA

  expect_true(anyNA(sf_na))
  expect_s3_class(sf_na, "sf")
  sf_na_check <- pull_crs(sf_na)

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
    sf_na_check,
    crs2,
    r2,
    v2,
    fromchar
  ))
})
