# Adapted from dplyr

test_that("bind_spat_rows() handles simple inputs", {
  df1 <- data.frame(x = 1:2, y = letters[1:2], lat = 1:2, lon = 1:2)
  df2 <- data.frame(x = 3:4, y = letters[3:4], lat = 1:2, lon = 1:2)
  df1 <- terra::vect(df1)
  df2 <- terra::vect(df2)
  out <- bind_spat_rows(df1, df2)
  expect_equal(as.data.frame(out), data.frame(x = 1:4, y = letters[1:4]))
})

test_that("bind_spat_rows() handles simple inputs with CRS", {
  df1 <- data.frame(x = 1:2, y = letters[1:2], lat = 1:2, lon = 1:2)
  df2 <- data.frame(x = 3:4, y = letters[3:4], lat = 1:2, lon = 1:2)
  df1 <- terra::vect(df1, crs = "EPSG:4326")
  df2 <- terra::vect(df2, crs = "EPSG:4326")
  out <- bind_spat_rows(df1, df2)
  expect_equal(as.data.frame(out), data.frame(x = 1:4, y = letters[1:4]))
})

test_that("bind_spat_rows() reorders columns to match first df", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(y = 1, x = 2)
  df1 <- terra::vect(df1, geom = c("x", "y"), keepgeom = TRUE)
  df2 <- terra::vect(df2, geom = c("x", "y"), keepgeom = TRUE)

  expect_named(bind_spat_rows(df1, df2), c("x", "y"))
})

test_that("bind_spat_rows() returns union of columns", {
  df1 <- data.frame(x = 1, lat = 1, lon = 1)
  df2 <- data.frame(y = 2, lat = 1, lon = 1)
  df1 <- terra::vect(df1)
  df2 <- terra::vect(df2)
  out <- bind_spat_rows(df1, df2)

  expect_equal(as.data.frame(out), data.frame(x = c(1, NA), y = c(NA, 2)))
})

test_that("bind_spat_rows() returns union of columns with CRS", {
  df1 <- data.frame(x = 1, lat = 1, lon = 1)
  df2 <- data.frame(y = 2, lat = 1, lon = 1)
  df1 <- terra::vect(df1, crs = "EPSG:4326")
  df2 <- terra::vect(df2, crs = "EPSG:4326")
  out <- bind_spat_rows(df1, df2)

  expect_equal(as.data.frame(out), data.frame(x = c(1, NA), y = c(NA, 2)))
})


test_that("bind_spat_rows() creates a column of identifiers", {
  df1 <- data.frame(x = 1:2, lat = 1:2, lon = 1:2)
  df2 <- data.frame(x = 3, lat = 1, lon = 1)
  df1 <- terra::vect(df1, crs = "EPSG:4326")
  df2 <- terra::vect(df2, crs = "EPSG:4326")


  # with
  out <- bind_spat_rows(a = df1, b = df2, .id = "id")
  expect_equal(
    as.data.frame(out),
    data.frame(id = c("a", "a", "b"), x = 1:3)
  )

  out <- bind_spat_rows(list(a = df1, b = df2), .id = "id")
  expect_equal(
    as.data.frame(out),
    data.frame(id = c("a", "a", "b"), x = 1:3)
  )

  # or without names
  out <- bind_spat_rows(df1, df2, .id = "id")
  expect_equal(
    as.data.frame(out),
    data.frame(id = c("1", "1", "2"), x = 1:3)
  )

  out <- bind_spat_rows(list(df1, df2), .id = "id")
  expect_equal(
    as.data.frame(out),
    data.frame(id = c("1", "1", "2"), x = 1:3)
  )
})


test_that("bind_spat_rows respects groups", {
  df <- data.frame(
    e = 1,
    f = factor(c(1, 1, 2, 2), levels = 1:3),
    g = c(1, 1, 2, 2),
    x = c(1, 2, 1, 4)
  )
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)
  df <- group_by(df, e, f, g, .drop = FALSE)

  gg <- bind_spat_rows(df, df)
  expect_equal(group_size(gg), c(4L, 4L, 0L))
})


# Column coercion --------------------------------------------------------------

test_that("bind_spat_rows() promotes integer to double", {
  df1 <- data.frame(a = 1L, b = 1L)
  df2 <- data.frame(a = 1, b = 1L)

  df1 <- terra::vect(df1, geom = c("a", "b"), keepgeom = TRUE)
  df2 <- terra::vect(df2, geom = c("a", "b"), keepgeom = TRUE)
  res <- bind_spat_rows(df1, df2)
  expect_type(res$a, "double")
  expect_type(res$b, "double")
})

# Geometry  handling ----------------------------------------------------------

test_that("bind_spat_rows() return empty point", {
  empt <- bind_spat_rows()
  expect_identical(
    as_tibble(empt, geom = "WKT"),
    as_tibble(terra::vect("POINT EMPTY"), geom = "WKT")
  )
})

test_that("bind_spat_rows() can bind SpatVectors", {
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  expect_silent(vend <- bind_spat_rows(v[1, ], v[2, ]))
  expect_s4_class(vend, "SpatVector")
})

test_that("bind_spat_rows() can bind SpatVectors and sf", {
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  sf2 <- as_sf(v[2, ])

  expect_silent(vend <- bind_spat_rows(v[1, ], sf2))
  expect_s4_class(vend, "SpatVector")
})

# Errors and messages ----------------------------------------------------------

test_that("bind_spat_rows() give informative errors", {
  expect_snapshot({
    "invalid .id"
    df1 <- data.frame(x = 1:3, lat = 1:3, lon = 1:3)
    df2 <- data.frame(x = 4:6, lat = 1:3, lon = 1:3)
    df1 <- terra::vect(df1)
    df2 <- terra::vect(df2)
    (expect_error(bind_spat_rows(df1, df2, .id = 5)))

    "invalid type"
    ll <- list(data.frame(a = 1:5))
    (expect_error(bind_spat_rows(ll)))

    (expect_error(bind_spat_rows(df1, ll)))
  })
})


test_that("bind_spat_rows() give informative message", {
  expect_snapshot({
    "different crs SpatVector"
    v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
    v1 <- v[1, ]
    v2 <- terra::project(v[2, ], "EPSG:3857")
    (expect_message(vend <- bind_spat_rows(v1, v2)))
    expect_s4_class(vend, "SpatVector")
    expect_identical(pull_crs(vend), pull_crs(v1))

    "different crs sf"
    v2_sf <- as_sf(v2)
    expect_s3_class(v2_sf, "sf")

    (expect_message(vend2 <- bind_spat_rows(v1, v2_sf)))
    expect_s4_class(vend2, "SpatVector")
    expect_identical(pull_crs(vend2), pull_crs(v1))

    "different crs sf and df"
    df1 <- data.frame(x = 1:3, lat = 1:3, lon = 1:3)

    (expect_message(vend3 <- bind_spat_rows(v1, v2_sf, df1)))
    expect_s4_class(vend3, "SpatVector")
    expect_identical(pull_crs(vend3), pull_crs(v1))
  })
})
