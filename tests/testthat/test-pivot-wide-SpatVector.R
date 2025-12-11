test_that("Back and fort", {
  skip_on_cran()

  nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nc <- nc[1:10, ]
  nc_pivoted <- pivot_longer(
    nc,
    dplyr::starts_with("BIR"),
    names_to = "year",
    values_to = "births"
  )

  nc_unpivot <- pivot_wider(
    nc_pivoted,
    values_from = "births",
    names_from = "year"
  )

  expect_s4_class(nc_unpivot, "SpatVector")

  # Reorder names
  nc_unpivot <- nc_unpivot[names(nc)]

  expect_identical(
    as_tbl_internal(nc),
    as_tbl_internal(nc_unpivot)
  )
})


test_that("Remove geometry from values", {
  skip_on_cran()

  nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nc <- nc[1:10, ]
  nc_pivoted <- pivot_longer(
    nc,
    dplyr::starts_with("BIR"),
    names_to = "year",
    values_to = "births"
  )

  expect_snapshot(
    nc_unpivot <- pivot_wider(
      nc_pivoted,
      values_from = c(births, geometry),
      names_from = "year"
    )
  )

  expect_s4_class(nc_unpivot, "SpatVector")

  # Reorder names
  nc_unpivot <- nc_unpivot[names(nc)]

  expect_identical(
    as_tbl_internal(nc),
    as_tbl_internal(nc_unpivot)
  )
})

test_that("Remove geometry from names", {
  skip_on_cran()

  nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nc <- nc[1:10, ]
  nc_pivoted <- pivot_longer(
    nc,
    dplyr::starts_with("BIR"),
    names_to = "year",
    values_to = "births"
  )

  expect_snapshot(
    nc_unpivot <- pivot_wider(
      nc_pivoted,
      values_from = births,
      names_from = c(geometry, year)
    )
  )

  expect_s4_class(nc_unpivot, "SpatVector")

  # Reorder names
  nc_unpivot <- nc_unpivot[names(nc)]

  expect_identical(
    as_tbl_internal(nc),
    as_tbl_internal(nc_unpivot)
  )
})

test_that("can pivot all cols to wide", {
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
    out <- pivot_wider(
      df,
      names_from = key,
      values_from = val,
      names_repair = "unique"
    )
  )
  expect_named(out, c("a...2", "a...3", "b"))
  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(out))
})

test_that("`names_repair` happens after spec column reorganization (#1107)", {
  skip_on_cran()

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
  skip_on_cran()

  df <- tibble::tibble(g = 1, k = "x", v = 2)
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  out <- df |>
    group_by(g) |>
    pivot_wider(names_from = k, values_from = v)
  expect_equal(dplyr::group_vars(out), "g")
})


test_that("`names_from` must be supplied if `name` isn't in `data` (#1240)", {
  skip_on_cran()

  df <- tibble::tibble(key = "x", val = 1)
  expect_snapshot((expect_error(pivot_wider(df, values_from = val))))
})


test_that("can use `names_expand` to get sorted and expanded column names", {
  skip_on_cran()

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


test_that("can fill only implicit missings from `names_expand`", {
  skip_on_cran()

  name1 <- factor(c(NA, "x"), levels = c("x", "y"))
  df <- tibble::tibble(name1 = name1, name2 = c("c", "d"), value = c(1, NA))
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  res <- pivot_wider(
    data = df,
    names_from = c(name1, name2),
    names_expand = TRUE,
    values_fill = list(value = 0)
  )

  expect_s4_class(res, "SpatVector")
  res_df <- as_tibble(res)
  attr(res_df, "crs") <- NULL

  # But not the explicit missing!
  expect_identical(
    res_df,
    tibble::tibble(
      x_c = 0,
      x_d = NA_real_,
      y_c = 0,
      y_d = 0,
      NA_c = 1,
      NA_d = 0
    )
  )
})

test_that("can override default keys, geometry sticky", {
  skip_on_cran()

  df <- tibble::tribble(
    ~row, ~name, ~var, ~value,
    1, "Sam", "age", 10,
    2, "Sam", "height", 1.5,
    3, "Bob", "age", 20,
  )

  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  pv <- pivot_wider(df, id_cols = name, names_from = var, values_from = value)
  expect_equal(nrow(pv), 2)
  expect_s4_class(pv, "SpatVector")
})

test_that("`id_cols = everything()` excludes `names_from` and `values_from`", {
  skip_on_cran()

  df <- tibble::tibble(key = "x", name = "a", value = 1L)
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")
  res <- pivot_wider(df, id_cols = dplyr::everything())
  expect_s4_class(res, "SpatVector")

  res_tbl <- as_tibble(res)
  attr(res_tbl, "crs") <- NULL

  expect_identical(
    res_tbl,
    tibble::tibble(key = "x", a = 1L)
  )
})


test_that("`id_expand` generates sorted rows even if no expansion is done", {
  skip_on_cran()

  df <- tibble::tibble(id = c(2, 1), name = c("a", "b"), value = c(1, 2))
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  res <- pivot_wider(df, id_expand = TRUE)
  expect_identical(res$id, c(1, 2))
})

test_that("`id_expand` does a cartesian expansion of `id_cols`", {
  skip_on_cran()

  df <- tibble::tibble(
    id1 = c(1, 2),
    id2 = c(3, 4),
    name = c("a", "b"),
    value = c(1, 2)
  )
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  res <- pivot_wider(df, id_expand = TRUE)
  expect_s4_class(res, "SpatVector")

  res_tbl <- as_tibble(res)
  attr(res_tbl, "crs") <- NULL

  expect_identical(
    res_tbl,
    tibble::tibble(
      id1 = c(1, 1, 2, 2),
      id2 = c(3, 4, 3, 4),
      a = c(1, NA, NA, NA),
      b = c(NA, NA, NA, 2),
    )
  )
})
