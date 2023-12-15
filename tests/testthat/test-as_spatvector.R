test_that("Error check", {
  as_tbl <- data.frame(
    x = 1:10,
    y = 1:10
  )

  expect_snapshot(as_spatvector(as_tbl), error = TRUE)
  expect_snapshot(as_spatvector(as_tbl, geom = NA), error = TRUE)
  expect_snapshot(as_spatvector(as_tbl, geom = c("a", "b", "c")), error = TRUE)
  expect_snapshot(as_spatvector(as_tbl, geom = 1), error = TRUE)
  # Not cli error, this is thrown due to no method for this
  expect_error(as_spatvector(as.matrix(as_tbl)))

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
  expect_message(as_spatvector(blanks, geom = "geom"))
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

test_that("Works with rowwise_df", {
  as_tbl <- data.frame(
    x = as.double(1:10),
    y = as.double(1:10),
    gr = rep_len(c("A", "B", "A"), length.out = 10)
  )



  gr <- dplyr::rowwise(as_tbl, gr)
  expect_true(is_rowwise_df(gr))


  gr_v <- as_spatvector(gr, geom = c("x", "y"), keepgeom = TRUE)

  expect_true(is_rowwise_spatvector(gr_v))

  # Remove attribute for comparision
  tbl_regen <- as_tibble(gr_v)

  attr(tbl_regen, "crs") <- NULL

  expect_identical(gr, tbl_regen)
})

test_that("Works with unnamed rowwise_df", {
  as_tbl <- data.frame(
    x = as.double(1:10),
    y = as.double(1:10),
    gr = rep_len(c("A", "B", "A"), length.out = 10)
  )



  gr <- dplyr::rowwise(as_tbl)
  expect_true(is_rowwise_df(gr))


  gr_v <- as_spatvector(gr, geom = c("x", "y"), keepgeom = TRUE)

  expect_true(is_rowwise_spatvector(gr_v))

  # Remove attribute for comparison
  tbl_regen <- as_tibble(gr_v)

  attr(tbl_regen, "crs") <- NULL

  expect_identical(gr, tbl_regen)
})
test_that("Works with sf", {
  skip("Review in > 0.5.1")
  sfobj <- sf::read_sf(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  expect_s3_class(sfobj, "sf")

  fromsf <- as_spatvector(sfobj)

  # Keep grouping
  sfobj_grouped <- dplyr::group_by(sfobj, iso2, first = substr(name, 1, 1))

  expect_true(dplyr::is_grouped_df(sfobj_grouped))

  fromsfgrouped <- as_spatvector(sfobj_grouped)

  expect_true(is_grouped_spatvector(fromsfgrouped))

  expect_identical(
    dplyr::group_data(sfobj_grouped),
    group_data(fromsfgrouped)
  )

  # Keep rowwise
  sfobj_rowwise <- dplyr::rowwise(sfobj)

  expect_true(is_rowwise_df(sfobj_rowwise))

  fromsfrowwise <- as_spatvector(sfobj_rowwise)

  expect_true(is_rowwise_spatvector(fromsfrowwise))

  expect_identical(
    dplyr::group_data(sfobj_rowwise),
    group_data(fromsfrowwise)
  )

  # Keep geoms even with other names
  sf2 <- sf::st_sf(x = 1, geom2 = sf::st_geometry(sfobj))
  expect_true(attr(sf2, "sf_column") == "geom2")

  fromsf2 <- as_spatvector(sf2)
})

test_that("Check sfc", {
  sfobj <- sf::read_sf(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  sfobj <- sf::st_geometry(sfobj)
  expect_s3_class(sfobj, "sfc")

  fromsf <- as_spatvector(sfobj)

  expect_equal(ncol(fromsf), 0)
})

test_that("Check sf with crs null", {
  sfobj <- sf::st_point(c(0, 0))
  sfobj <- sf::st_sfc(sfobj)

  expect_true(is.na(pull_crs(sfobj)))

  fromsf <- as_spatvector(sfobj)

  expect_true(is.na(pull_crs(fromsf)))
})

test_that("Check sf with empty geoms: POLYGONS", {
  sfobj <- sf::read_sf(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  sfobj <- dplyr::bind_rows(sfobj, data.frame(a = 1))

  expect_true(any(sf::st_is_empty(sfobj)))

  assp <- as_spatvector(sfobj)

  expect_equal(terra::geomtype(assp), "polygons")

  # Can convert back to sf

  expect_silent(sf::st_as_sf(assp))
  expect_true(any(sf::st_is_empty(sf::st_as_sf(assp))))
})

test_that("Check sf with empty geoms: LINESTRINGS", {
  sfobj <- sf::read_sf(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  sfobj <- sf::st_cast(sfobj, "MULTILINESTRING", warn = FALSE)
  sfobj <- dplyr::bind_rows(sfobj, data.frame(a = 1))

  expect_true(any(sf::st_is_empty(sfobj)))

  assp <- as_spatvector(sfobj)

  expect_equal(terra::geomtype(assp), "lines")

  # Can convert back to sf

  expect_silent(sf::st_as_sf(assp))
  expect_true(any(sf::st_is_empty(sf::st_as_sf(assp))))
})

test_that("Check sf with empty geoms: POINTS", {
  sfobj <- sf::read_sf(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  sfobj <- sf::st_cast(sfobj[1:2, ], "MULTIPOINT", warn = FALSE)
  sfobj <- dplyr::bind_rows(sfobj, data.frame(a = 1))

  expect_true(any(sf::st_is_empty(sfobj)))

  assp <- as_spatvector(sfobj)

  expect_equal(terra::geomtype(assp), "points")

  # Can convert back to sf

  expect_silent(sf::st_as_sf(assp))
  expect_true(any(sf::st_is_empty(sf::st_as_sf(assp))))
})

test_that("Check internal", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)



  # Test bypass
  bypass_v <- as_spat_internal(v)
  expect_identical(
    as_tbl_internal(v),
    as_tbl_internal(bypass_v)
  )

  # From internal
  tbl <- as_tbl_internal(v)
  expect_silent(as_spat_internal(tbl))



  v2 <- as_spat_internal(tbl)

  expect_identical(
    as_tbl_internal(v),
    as_tbl_internal(v2)
  )

  # Now remove attribs
  tbl2 <- tbl

  att <- attributes(tbl2)
  attributes(tbl2) <- NULL
  names(tbl2) <- att$names
  tbl2 <- as.data.frame(tbl2)

  expect_error(as_spat_internal(tbl2))
})


test_that("Check internal grouped", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  v$gr <- rep_len(c("A", "A", "B"), length.out = nrow(v))

  gr_v <- group_by(v, gr)


  gr_tbl <- as_tbl_internal(gr_v)

  # Regen
  gr_tbl_regen <- as_spat_internal(gr_tbl)

  expect_true(is_grouped_spatvector(gr_tbl_regen))

  expect_identical(
    as_tbl_internal(gr_v),
    as_tbl_internal(gr_tbl_regen)
  )

  # Should match also with groups on gr_tbl

  expect_true(dplyr::is_grouped_df(gr_tbl))

  expect_identical(
    dplyr::group_data(gr_tbl),
    dplyr::group_data(as_tbl_internal(gr_tbl_regen))
  )
})

test_that("Check internal rowwise", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  v$gr <- rep_len(c("A", "A", "B"), length.out = nrow(v))

  gr_v <- rowwise(v, gr)


  gr_tbl <- as_tbl_internal(gr_v)

  # Regen
  gr_tbl_regen <- as_spat_internal(gr_tbl)

  expect_true(is_rowwise_spatvector(gr_tbl_regen))

  expect_identical(
    as_tbl_internal(gr_v),
    as_tbl_internal(gr_tbl_regen)
  )

  # Should match also with groups on gr_tbl

  expect_true(is_rowwise_df(gr_tbl))

  expect_identical(
    dplyr::group_data(gr_tbl),
    dplyr::group_data(as_tbl_internal(gr_tbl_regen))
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
  pol_df <- as_tbl_internal(pol)

  # Add NA and "" geom
  pol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  pol_df$is_empty <- FALSE
  pol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newpol <- as_spat_internal(pol_df)
  expect_equal(terra::geomtype(newpol), "polygons")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )


  # MULTIPOLYGON
  mpol_df <- as_tbl_internal(mpol)

  # Add NA and "" geom
  mpol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  mpol_df$is_empty <- FALSE
  mpol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newmpol <- as_spat_internal(mpol_df)
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
  mpol <- terra::as.lines(v[1:3, ])
  mpol_wkt <- terra::geom(mpol, wkt = TRUE)
  # Check that we got that right
  expect_false(any(grepl("MULTI", pol_wkt)))
  expect_true(any(grepl("MULTI", mpol_wkt)))

  # LINESTRING
  pol_df <- as_tbl_internal(pol)

  # Add NA and "" geom
  pol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  pol_df$is_empty <- FALSE
  pol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newpol <- as_spat_internal(pol_df)
  expect_equal(terra::geomtype(newpol), "lines")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )


  # MULTILINESTRING
  mpol_df <- as_tbl_internal(mpol)

  # Add NA and "" geom
  mpol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  mpol_df$is_empty <- FALSE
  mpol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newmpol <- as_spat_internal(mpol_df)
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
  mpol <- terra::as.points(v[1:3, ])
  # need to aggregate

  mpol <- terra::aggregate(mpol, by = "iso2")
  mpol_wkt <- terra::geom(mpol, wkt = TRUE)

  # Check that we got that right
  expect_false(any(grepl("MULTI", pol_wkt)))
  expect_true(any(grepl("MULTI", mpol_wkt)))

  # POINT
  pol_df <- as_tbl_internal(pol)

  # Add NA and "" geom
  pol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  pol_df$is_empty <- FALSE
  pol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newpol <- as_spat_internal(pol_df)
  expect_equal(terra::geomtype(newpol), "points")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )


  # MULTIPOINT
  mpol_df <- as_tbl_internal(mpol)

  # Add NA and "" geom
  mpol_df$geometry[1:2] <- c(NA, "")

  # Add mark
  mpol_df$is_empty <- FALSE
  mpol_df$is_empty[1:2] <- TRUE

  # Reconstruct
  newmpol <- as_spat_internal(mpol_df)
  expect_equal(terra::geomtype(newmpol), "points")

  # Check conversion to sf
  tosf <- sf::st_as_sf(newmpol)
  expect_identical(
    sf::st_is_empty(tosf),
    tosf$is_empty
  )
})


test_that("Keep group with NULL", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- sf::read_sf(f)
  v$gr <- rep_len(c("A", "B", "C", "D"), length.out = nrow(v))

  # Add null
  vend <- dplyr::bind_rows(v, data.frame(gr = "E"))
  pol_g <- group_by(vend, gr)

  expect_s3_class(pol_g, "sf")
  # Check that has empty geoms
  expect_true(any(sf::st_is_empty(pol_g)))

  # Has groups
  sfg_data <- group_data(pol_g)

  # Convert to spatvector
  sv_gr <- as_spatvector(pol_g)


  expect_identical(sfg_data, group_data(sv_gr))

  # Can convert back to sf
  back_sf <- as_sf(sv_gr)

  expect_identical(group_data(sv_gr), group_data(back_sf))
})
