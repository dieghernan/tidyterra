test_that("Error check", {
  as_tbl <- data.frame(
    x = 1:10,
    y = 1:10
  )

  expect_error(as_spatvector(as_tbl))
  expect_error(as_spatvector(as_tbl, geom = NA))
  expect_error(as_spatvector(as_tbl, geom = c("a", "b", "c")))
  expect_error(as_spatvector(as_tbl, geom = 1))
  expect_error(as_spatvector(as.matrix(as_tbl)),
    regexp = "should be a data.frame"
  )
  expect_silent(as_spatvector(as_tbl, geom = c("x", "y"), crs = "EPSG:4326"))
})


test_that("Handle NAs", {
  as_tbl <- data.frame(
    x = 1:10,
    y = 1:10,
    geom = rep_len("POINT(0 0)", length.out = 10)
  )

  with_nas <- as_tbl
  with_nas[8, ] <- NA

  expect_message(as_spatvector(with_nas, geom = c("x", "y")))
  expect_message(as_spatvector(with_nas, geom = "geom"))

  chars <- with_nas
  chars$x <- as.character(chars$x)
  chars$y <- as.character(chars$y)

  expect_message(as_spatvector(chars, geom = c("x", "y")))

  # With blanks instead
  blanks <- chars
  blanks[8, ] <- ""

  expect_message(as_spatvector(blanks, geom = c("x", "y")))
  expect_message(as_spatvector(blanks, geom = c("geom")))
})


test_that("Regenerate vector properly with WKT", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)


  # Do nothing if r is SpatVector
  expect_s4_class(v, "SpatVector")

  bypass_v <- as_spatvector(v)

  expect_s4_class(bypass_v, "SpatVector")

  tib <- as_tibble(v, geom = "WKT")

  expect_s3_class(tib, "tbl")
  regen <- as_spatvector(tib, geom = "geometry")

  expect_s4_class(regen, "SpatVector")

  expect_identical(pull_crs(v), pull_crs(regen))

  # Compare values
  expect_identical(
    as_tibble(v),
    as_tibble(regen)
  )

  # If nothing provided
  noatr <- tib
  attr(noatr, "crs") <- NULL

  fromnonatr <- as_spatvector(noatr, geom = "geometry")

  expect_false(identical(
    as_tibble(fromnonatr),
    as_tibble(v)
  ))

  expect_s4_class(fromnonatr, "SpatVector")

  expect_true(is.na(pull_crs(fromnonatr)))
})

test_that("Regenerate vector properly with lon,lat", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  v <- terra::centroids(v)

  # Do nothing if r is SpatVector
  expect_s4_class(v, "SpatVector")

  bypass_v <- as_spatvector(v)

  expect_s4_class(bypass_v, "SpatVector")

  tib <- as_tibble(v, geom = "XY")

  expect_s3_class(tib, "tbl")
  regen <- as_spatvector(tib, geom = c("x", "y"))

  expect_s4_class(regen, "SpatVector")

  expect_identical(pull_crs(v), pull_crs(regen))

  # Compare values
  expect_identical(
    as_tibble(v),
    as_tibble(regen)
  )

  # If nothing provided
  noatr <- tib
  attr(noatr, "crs") <- NULL

  fromnonatr <- as_spatvector(noatr, geom = c("x", "y"))

  expect_false(identical(
    as_tibble(fromnonatr),
    as_tibble(v)
  ))

  expect_s4_class(fromnonatr, "SpatVector")

  expect_true(is.na(pull_crs(fromnonatr)))
})

test_that("Works with grouped_df", {
  as_tbl <- data.frame(
    x = as.double(1:10),
    y = as.double(1:10),
    gr = rep_len(c("A", "B", "A"), length.out = 10)
  )



  gr <- dplyr::group_by(as_tbl, gr)
  expect_true(dplyr::is_grouped_df(gr))


  gr_v <- as_spatvector(gr, geom = c("x", "y"), keepgeom = TRUE)

  expect_true(is_grouped_spatvector(gr_v))

  # Remove attribute for comparision
  tbl_regen <- as_tibble(gr_v)

  attr(tbl_regen, "crs") <- NULL

  expect_identical(gr, tbl_regen)
})


test_that("Check internal", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)



  # Test bypass
  bypass_v <- as_spatvect_attr(v)
  expect_identical(
    as_tbl_spatvect_attr(v),
    as_tbl_spatvect_attr(bypass_v)
  )

  # From internal
  tbl <- as_tbl_spatvect_attr(v)
  expect_silent(as_spatvect_attr(tbl))



  v2 <- as_spatvect_attr(tbl)

  expect_identical(
    as_tbl_spatvect_attr(v),
    as_tbl_spatvect_attr(v2)
  )

  # Now remove attribs
  tbl2 <- tbl

  att <- attributes(tbl2)
  attributes(tbl2) <- NULL
  names(tbl2) <- att$names
  tbl2 <- as.data.frame(tbl2)

  expect_message(as_spatvect_attr(tbl2))

  v_noattr <- as_spatvect_attr(tbl2)

  expect_s4_class(
    v_noattr,
    "SpatVector"
  )

  expect_false(identical(
    as_tbl_spatvect_attr(v),
    as_tbl_spatvect_attr(v_noattr)
  ))
})


test_that("Check internal grouped", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  v$gr <- rep_len(c("A", "A", "B"), length.out = nrow(v))

  gr_v <- group_by(v, gr)


  gr_tbl <- as_tbl_spatvect_attr(gr_v)

  # Regen
  gr_tbl_regen <- as_spatvect_attr(gr_tbl)

  expect_true(is_grouped_spatvector(gr_tbl_regen))

  expect_identical(
    as_tbl_spatvect_attr(gr_v),
    as_tbl_spatvect_attr(gr_tbl_regen)
  )

  # Should match also with groups on gr_tbl

  expect_true(dplyr::is_grouped_df(gr_tbl))

  expect_identical(
    dplyr::group_data(gr_tbl),
    dplyr::group_data(as_tbl_spatvect_attr(gr_tbl_regen))
  )
})


test_that("Check internal NULL: POLYGONS", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  pol <- terra::disagg(v[1:3, ])
  pol_wkt <- terra::geom(pol, wkt = TRUE)
  mpol <- v[1:3, ]
  mpol_wkt <- terra::geom(mpol, wkt = TRUE)
  # Check that we got that right
  expect_false(any(grepl("MULTI", pol_wkt)))
  expect_true(any(grepl("MULTI", mpol_wkt)))

  # POLYGON
  pol_df <- as_tbl_spatvect_attr(pol)

  # Add NA and "" geom
  pol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  pol_df$is_empty <- FALSE
  pol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newpol <- as_spatvect_attr(pol_df)
  expect_equal(terra::geomtype(newpol), "polygons")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )


  # MULTIPOLYGON
  mpol_df <- as_tbl_spatvect_attr(mpol)

  # Add NA and "" geom
  mpol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  mpol_df$is_empty <- FALSE
  mpol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newmpol <- as_spatvect_attr(mpol_df)
  expect_equal(terra::geomtype(newmpol), "polygons")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newmpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )
})


test_that("Check internal NULL: LINES", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  pol <- terra::disagg(v[1:3, ], segments = TRUE)
  pol_wkt <- terra::geom(pol, wkt = TRUE)
  mpol <- as.lines(v[1:3, ])
  mpol_wkt <- terra::geom(mpol, wkt = TRUE)
  # Check that we got that right
  expect_false(any(grepl("MULTI", pol_wkt)))
  expect_true(any(grepl("MULTI", mpol_wkt)))

  # LINESTRING
  pol_df <- as_tbl_spatvect_attr(pol)

  # Add NA and "" geom
  pol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  pol_df$is_empty <- FALSE
  pol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newpol <- as_spatvect_attr(pol_df)
  expect_equal(terra::geomtype(newpol), "lines")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )


  # MULTILINESTRING
  mpol_df <- as_tbl_spatvect_attr(mpol)

  # Add NA and "" geom
  mpol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  mpol_df$is_empty <- FALSE
  mpol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newmpol <- as_spatvect_attr(mpol_df)
  expect_equal(terra::geomtype(newmpol), "lines")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newmpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )
})


test_that("Check internal NULL: POINTS", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  pol <- terra::centroids(terra::disagg(v[1:3, ]))
  pol_wkt <- terra::geom(pol, wkt = TRUE)
  mpol <- as.points(v[1:3, ])
  # need to aggregate

  mpol <- terra::aggregate(mpol, by = "iso2")
  mpol_wkt <- terra::geom(mpol, wkt = TRUE)

  # Check that we got that right
  expect_false(any(grepl("MULTI", pol_wkt)))
  expect_true(any(grepl("MULTI", mpol_wkt)))

  # POINT
  pol_df <- as_tbl_spatvect_attr(pol)

  # Add NA and "" geom
  pol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  pol_df$is_empty <- FALSE
  pol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newpol <- as_spatvect_attr(pol_df)
  expect_equal(terra::geomtype(newpol), "points")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )


  # MULTIPOINT
  mpol_df <- as_tbl_spatvect_attr(mpol)

  # Add NA and "" geom
  mpol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  mpol_df$is_empty <- FALSE
  mpol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newmpol <- as_spatvect_attr(mpol_df)
  expect_equal(terra::geomtype(newmpol), "points")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newmpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )
})
