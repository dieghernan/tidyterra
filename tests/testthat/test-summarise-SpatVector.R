test_that("Summarise gives the same results than default method", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nogroup <- summarise(
    v,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74)
  )
  nogroup_df <- summarise(
    as_tibble(v),
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74)
  )

  expect_s4_class(nogroup, "SpatVector")
  expect_s3_class(nogroup_df, "tbl")
  expect_true(all(nogroup_df == as_tibble(nogroup)))

  g <- group_by(v, SID74, SID79)
  g_df <- group_by(as_tibble(v), SID74, SID79)

  g_summ <- summarise(
    g,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74)
  )

  g_summ_df <- summarise(
    g_df,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74)
  )

  expect_s4_class(g_summ, "SpatVector")
  expect_s3_class(g_summ_df, "tbl")
  expect_true(all(g_summ_df == as_tibble(g_summ)))
})

test_that("Summarise preserve CRS", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nogroup <- summarise(
    v,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74)
  )
  expect_s4_class(nogroup, "SpatVector")
  expect_identical(pull_crs(v), pull_crs(nogroup))
})

test_that("Summarise handles dissolve", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  diss <- summarise(
    v,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74),
    .dissolve = TRUE
  )
  expect_s4_class(diss, "SpatVector")

  dissolved_pols <- terra::disagg(diss)

  nodiss <- summarise(
    v,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74),
    .dissolve = FALSE
  )
  expect_s4_class(nodiss, "SpatVector")

  nodissolved_pols <- terra::disagg(nodiss)

  # Same statistics
  expect_identical(as_tibble(diss), as_tibble(nodiss))

  expect_gt(nrow(nodissolved_pols), nrow(dissolved_pols))
})


test_that("Summarise handles dissolve on groups", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  v_g <- group_by(v, SID74, SID79)
  diss <- summarise(
    v_g,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74),
    .dissolve = TRUE
  )
  expect_s4_class(diss, "SpatVector")

  dissolved_pols <- terra::disagg(diss)

  nodiss <- summarise(
    v_g,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74),
    .dissolve = FALSE
  )
  expect_s4_class(nodiss, "SpatVector")

  nodissolved_pols <- terra::disagg(nodiss)

  # Same statistics
  expect_identical(as_tibble(diss), as_tibble(nodiss))

  # Distinct polygons
  expect_gt(nrow(nodissolved_pols), nrow(dissolved_pols))

  # Groups preserved?
  expect_true(is_grouped_spatvector(diss))
  expect_true(is_grouped_spatvector(nodiss))

  # Should have move one layer, check groups and results with df
  df <- group_by(as_tibble(v), SID74, SID79)
  expect_true(dplyr::is_grouped_df(df))
  expect_identical(group_data(df), group_data(v_g))

  diss_df <- summarise(
    df,
    sum_all = sum(AREA),
    n_all = dplyr::n(),
    mean = mean(BIR74)
  )

  expect_s3_class(diss_df, "tbl")
  expect_true(all(diss_df == as_tibble(diss)))
  expect_true(all(diss_df == as_tibble(nodiss)))

  expect_true(dplyr::is_grouped_df(diss_df))
  expect_identical(group_data(diss_df), group_data(diss))
  expect_identical(group_data(diss_df), group_data(nodiss))
})

test_that("Check aggregation: POINTS", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v <- terra::centroids(v)
  v$gr <- rep(c("A", "B", "C"), 3)
  v$nn <- seq_len(nrow(v))

  expect_identical(terra::geomtype(v), "points")

  # Ungrouped
  # Dissolve
  v_ds <- summarise(v, s = sum(nn), .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = TRUE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Non dissolved
  v_ds <- summarise(v, s = sum(nn), .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Grouped
  # Dissolve
  v_ds <- summarise(group_by(v, gr), s = sum(nn), .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # No Dissolve
  v_ds <- summarise(group_by(v, gr), s = sum(nn), .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)
})


test_that("Check aggregation: POLYGONS", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep(c("A", "B", "C"), 3)
  v$nn <- seq_len(nrow(v))

  expect_identical(terra::geomtype(v), "polygons")

  # Ungrouped
  # Dissolve
  v_ds <- summarise(v, s = sum(nn), .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = TRUE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  g_test <- g_wkt

  # Non dissolved
  v_ds <- summarise(v, s = sum(nn), .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  expect_false(identical(g_test, g_wkt))

  # Grouped
  # Dissolve
  v_ds <- summarise(group_by(v, gr), s = sum(nn), .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  g_test <- g_wkt

  # No Dissolve
  v_ds <- summarise(group_by(v, gr), s = sum(nn), .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  expect_false(identical(g_test, g_wkt))
})


test_that("Check aggregation: LINES", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep(c("A", "B", "C"), 3)
  v$nn <- seq_len(nrow(v))

  v <- terra::as.lines(v)

  expect_identical(terra::geomtype(v), "lines")

  # Ungrouped
  # Dissolve
  v_ds <- summarise(v, s = sum(nn), .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = TRUE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  g_test <- g_wkt

  # Non dissolved
  v_ds <- summarise(v, s = sum(nn), .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  expect_false(identical(g_test, g_wkt))

  # Grouped
  # Dissolve
  v_ds <- summarise(group_by(v, gr), s = sum(nn), .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  g_test <- g_wkt

  # No Dissolve
  v_ds <- summarise(group_by(v, gr), s = sum(nn), .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  expect_false(identical(g_test, g_wkt))
})


# .by ----------------------------------------------------------------------

test_that("can group transiently using `.by`", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))
  df <- dplyr::tibble(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))

  df_v <- cbind(v[seq_len(nrow(df)), 0], df)

  out <- summarise(df_v, x = mean(x), .by = g)

  expect_identical(out$g, c(1, 2))
  expect_identical(out$x, c(3, 2))
  expect_s4_class(out, class(v))
})


test_that("transient grouping orders by first appearance", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  df <- tibble(g = c(2, 1, 2, 0), x = c(4, 2, 8, 5))
  df_v <- cbind(v[seq_len(nrow(df)), 0], df)

  out <- summarise(df, x = mean(x), .by = g)

  expect_identical(out$g, c(2, 1, 0))
  expect_identical(out$x, c(6, 2, 5))
})

test_that("can't use `.by` with `.groups`", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))
  v_g <- group_by(v, CNTY_ID)

  expect_error(
    summarise(v_g, .by = x, .groups = "drop")
  )
})

test_that("catches `.by` with grouped-df", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))
  df <- tibble(x = 1)
  df_v <- cbind(v[1, ], df)

  gdf <- group_by(df_v, x)

  expect_error(
    summarise(gdf, .by = x)
  )
})

test_that("catches `.by` with rowwise-df", {
  skip_on_cran()
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))
  df <- tibble(x = 1)
  df_v <- cbind(v[1, ], df)
  rdf <- rowwise(df_v)

  expect_error(
    summarise(rdf, .by = x)
  )
})
