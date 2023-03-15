# Adapted from dplyr
test_that("bind_spat_cols() handles empty argument", {
  expect_equal(
    as.data.frame(bind_spat_cols(), geom = "WKT"),
    data.frame(geometry = "POINT EMPTY")
  )
})


test_that("bind_spat_cols() repairs names", {
  df <- data.frame(a = 1, b = 2)
  df <- terra::vect(df, geom = c("a", "b"), keepgeom = TRUE)
  expect_snapshot(bound <- bind_spat_cols(df, df))


  expect_s4_class(bound, "SpatVector")

  expect_message(
    repaired <- tibble::as_tibble(
      data.frame(
        a = 1, b = 2, a = 1, b = 2,
        check.names = FALSE
      ),
      .name_repair = "unique"
    ), "New names"
  )

  expect_identical(names(bound), names(repaired))
  expect_equal(as.data.frame(bound), as.data.frame(repaired))
})

test_that("bind_spat_cols() honours .name_repair=", {
  aa <- terra::vect("POINT (0 0)")
  aa <- bind_spat_cols(aa, data.frame(a = 1))


  expect_message(res <- bind_spat_cols(
    aa, data.frame(a = 2)
  ))
  expect_equal(as.data.frame(res), data.frame(a...1 = 1, a...2 = 2))

  expect_error(bind_spat_cols(
    .name_repair = "check_unique",
    aa, data.frame(a = 2)
  ))
})

test_that("bind_spat_cols() accepts NULL", {
  df1 <- data.frame(a = 1:10, b = 1:10)
  df2 <- data.frame(c = 1:10, d = 1:10)

  df1 <- terra::vect(df1,
    geom = c("a", "b"), keepgeom = TRUE,
    crs = "EPSG:4326"
  )

  res1 <- bind_spat_cols(df1, df2)
  res2 <- bind_spat_cols(df1, NULL, df2)
  res3 <- bind_spat_cols(df1, df2, NULL)

  expect_identical(as_tbl_internal(res1), as_tbl_internal(res2))
  expect_identical(as_tbl_internal(res1), as_tbl_internal(res3))
})

test_that("bind_spat_cols() accepts sf", {
  df1 <- data.frame(a = 1:10, b = 1:10)

  df1 <- terra::vect(df1,
    geom = c("a", "b"), keepgeom = TRUE,
    crs = "EPSG:4326"
  )

  sfobj <- as_sf(df1[, 2])
  names(sfobj) <- c("zz", "geometry")

  expect_s3_class(sfobj, "sf")

  expect_silent(sv <- bind_spat_cols(df1, sfobj))
  expect_s4_class(sv, "SpatVector")
})


test_that("bind_spat_cols respects groups", {
  df_init <- data.frame(
    e = 1,
    f = factor(c(1, 1, 2, 2), levels = 1:3),
    g = c(1, 1, 2, 2),
    x = c(1, 2, 1, 4)
  )
  df <- terra::vect(df_init, geom = c("g", "x"), keepgeom = TRUE)
  df <- group_by(df, e, f, g, .drop = FALSE)
  df2 <- data.frame(ss = 1:4)
  gg <- bind_spat_cols(df, df2)
  expect_identical(group_size(df), group_size(gg))
  expect_identical(group_vars(df), group_vars(gg))

  df_init <- dplyr::group_by(df_init, e, f, g, .drop = FALSE)
  gg_df <- dplyr::bind_cols(df_init, df2)
  expect_identical(group_vars(gg), group_vars(gg_df))
})


test_that("bind_spat_cols() gives informative errors", {
  a <- terra::vect("POINT (0 0)")
  a <- bind_spat_cols(a, data.frame(a = 1))
  expect_snapshot({
    "# incompatible size"
    (expect_error(bind_spat_cols(mtcars)))
  })
})
