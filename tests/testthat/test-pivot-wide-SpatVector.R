test_that("Back and fort", {
  nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nc <- nc[1:10, ]
  nc_pivoted <- pivot_longer(nc, dplyr::starts_with("BIR"),
    names_to = "year",
    values_to = "births"
  )

  nc_unpivot <- pivot_wider(nc_pivoted,
    values_from = "births",
    names_from = "year"
  )

  expect_s4_class(nc_unpivot, "SpatVector")

  # Reorder names
  nc_unpivot <- nc_unpivot[names(nc)]

  expect_identical(
    as_tbl_internal(nc), as_tbl_internal(nc_unpivot)
  )
})


test_that("Remove geometry from values", {
  nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nc <- nc[1:10, ]
  nc_pivoted <- pivot_longer(nc, dplyr::starts_with("BIR"),
    names_to = "year",
    values_to = "births"
  )

  expect_snapshot(
    nc_unpivot <- pivot_wider(nc_pivoted,
      values_from = c(births, geometry),
      names_from = "year"
    )
  )


  expect_s4_class(nc_unpivot, "SpatVector")

  # Reorder names
  nc_unpivot <- nc_unpivot[names(nc)]

  expect_identical(
    as_tbl_internal(nc), as_tbl_internal(nc_unpivot)
  )
})

test_that("Remove geometry from names", {
  nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nc <- nc[1:10, ]
  nc_pivoted <- pivot_longer(nc, dplyr::starts_with("BIR"),
    names_to = "year",
    values_to = "births"
  )

  expect_snapshot(
    nc_unpivot <- pivot_wider(nc_pivoted,
      values_from = births,
      names_from = c(geometry, year)
    )
  )


  expect_s4_class(nc_unpivot, "SpatVector")

  # Reorder names
  nc_unpivot <- nc_unpivot[names(nc)]

  expect_identical(
    as_tbl_internal(nc), as_tbl_internal(nc_unpivot)
  )
})

test_that("can pivot all cols to wide", {
  df <- tibble::tibble(key = c("x", "y", "z"), val = 1:3)

  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_s4_class(df, "SpatVector")


  pv <- pivot_wider(df, names_from = key, values_from = val)

  expect_named(pv, c("x", "y", "z"))
  expect_equal(nrow(pv), 1)
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("non-pivoted cols are preserved", {
  df <- tibble::tibble(a = 1, key = c("x", "y"), val = 1:2)
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_s4_class(df, "SpatVector")

  pv <- pivot_wider(df, names_from = key, values_from = val)

  expect_named(pv, c("a", "x", "y"))
  expect_equal(nrow(pv), 1)
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("implicit missings turn into explicit missings", {
  df <- tibble::tibble(a = 1:2, key = c("x", "y"), val = 1:2)
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_s4_class(df, "SpatVector")
  pv <- pivot_wider(df, names_from = key, values_from = val)

  expect_equal(pv$a, c(1, 2))
  expect_equal(pv$x, c(1, NA))
  expect_equal(pv$y, c(NA, 2))
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("error when overwriting existing column", {
  df <- tibble::tibble(
    a = c(1, 1),
    key = c("a", "b"),
    val = c(1, 2)
  )
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_error(pivot_wider(df, names_from = key, values_from = val))


  expect_snapshot(
    out <- pivot_wider(df,
      names_from = key, values_from = val,
      names_repair = "unique"
    )
  )
  expect_named(out, c("a...1", "a...3", "b"))
  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(out))
})

test_that("`names_repair` happens after spec column reorganization (#1107)", {
  df <- tibble::tibble(
    test = c("a", "b"),
    name = c("test", "test2"),
    value = c(1, 2)
  )
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  out <- pivot_wider(df, names_repair = ~ make.unique(.x))

  expect_identical(out$test, c("a", "b"))
  expect_identical(out$test.1, c(1, NA))
  expect_identical(out$test2, c(NA, 2))
  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(out))
})

test_that("grouping is preserved", {
  df <- tibble::tibble(g = 1, k = "x", v = 2)
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  out <- df %>%
    group_by(g) %>%
    pivot_wider(names_from = k, values_from = v)
  expect_equal(dplyr::group_vars(out), "g")
})




test_that("`names_from` must be supplied if `name` isn't in `data` (#1240)", {
  df <- tibble::tibble(key = "x", val = 1)
  expect_snapshot((expect_error(pivot_wider(df, values_from = val))))
})



test_that("can use `names_expand` to get sorted and expanded column names", {
  name1 <- factor(c(NA, "x"), levels = c("x", "y"))

  df <- tibble::tibble(name1 = name1, name2 = c("c", "d"), value = c(1, 2))
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  pivoted <- pivot_wider(df, names_from = c(name1, name2), names_expand = TRUE)

  tib <- as_tibble(pivoted)
  attr(tib, "crs") <- NULL
  na <- NA_real_

  expect_identical(
    tib,
    tibble::tibble(x_c = na, x_d = 2, y_c = na, y_d = na, NA_c = 1, NA_d = na)
  )
})
