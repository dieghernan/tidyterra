# From dplyr
# count -------------------------------------------------------------------

test_that("count sorts output by keys by default", {
  skip_on_cran()

  # Due to usage of `summarise()` internally
  df <- tibble(x = c(2, 1, 1, 2, 1))

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  expect_s4_class(df, "SpatVector")
  out <- count(df, x)
  expect_equal(as_tibble(out)$x, c(1, 2))
  expect_equal(as_tibble(out)$n, c(3, 2))
})

test_that("count can sort output by `n`", {
  skip_on_cran()

  df <- tibble(x = c(1, 1, 2, 2, 2))
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  out <- count(df, x, sort = TRUE)
  expect_equal(as_tibble(out)$x, c(2, 1))
  expect_equal(as_tibble(out)$n, c(3, 2))
})


test_that("informs if n column already present, unless overridden", {
  skip_on_cran()

  df1 <- data.frame(n = c(1, 1, 2, 2, 2))
  df1$lon <- df1$n
  df1$lat <- df1$n

  df1 <- terra::vect(df1)

  expect_message(out <- count(df1, n), "already present")
  expect_s4_class(out, "SpatVector")
  expect_named(out, c("n", "nn"))

  # not a good idea, but supported
  expect_message(out <- count(df1, n, name = "n"), NA)
  expect_named(out, "n")

  expect_message(out <- count(df1, n, name = "nn"), NA)
  expect_named(out, c("n", "nn"))

  df2 <- tibble(n = c(1, 1, 2, 2, 2), nn = 1:5)
  df2$lon <- df2$n
  df2$lat <- df2$n
  df2 <- terra::vect(df2)

  expect_message(out <- count(df2, n), "already present")
  expect_named(out, c("n", "nn"))
  expect_s4_class(df2, "SpatVector")
  expect_message(out <- count(df2, n, nn), "already present")
  expect_named(out, c("n", "nn", "nnn"))
})

test_that("name must be string", {
  skip_on_cran()

  df1 <- data.frame(x = c(1, 1, 2, 2, 2))
  df1$lon <- df1$x
  df1$lat <- df1$x
  df1 <- terra::vect(df1)

  expect_snapshot(error = TRUE, count(df1, x, name = 1))
  expect_snapshot(error = TRUE, count(df1, x, name = letters))
})

test_that(".drop argument deprecated", {
  skip_on_cran()

  df <- terra::vect(tibble(lat = 1, lon = 1))
  df <- cbind(df, tibble(f = factor("b", levels = c("a", "b", "c"))))

  expect_snapshot(res <- count(df, f, .drop = FALSE))
  res2 <- count(df, f)

  expect_identical(as_tibble(res), as_tibble(res2))
})

test_that("output preserves grouping", {
  skip_on_cran()

  df <- data.frame(g = c(1, 2, 2, 2))
  df$lon <- 1:4
  df$lat <- 1:4
  df <- terra::vect(df)
  exp <- data.frame(g = c(1, 2), n = c(1, 3))
  exp$lon <- 1:2
  exp$lat <- 1:2
  exp <- terra::vect(exp)

  expect_equal(
    df |> count(g) |> as_tibble(),
    exp |> as_tibble()
  )

  expect_equal(
    df |> group_by(g) |> count() |> as_tibble(),
    exp |> group_by(g) |> as_tibble()
  )
})

test_that("output preserves class & attributes where possible", {
  skip_on_cran()

  df <- data.frame(g = c(1, 2, 2, 2))
  df$lon <- 1:4
  df$lat <- 1:4
  df <- terra::vect(df, crs = "EPSG:4326")

  out <- df |> count(g)
  expect_s4_class(out, "SpatVector")
  expect_identical(group_vars(out), group_vars(df))
  expect_identical(pull_crs(out), pull_crs(df))

  out <- df |>
    group_by(g) |>
    count()
  expect_s4_class(out, "SpatVector")
  expect_equal(group_vars(out), "g")
  expect_identical(pull_crs(out), pull_crs(df))
})

test_that("can only explicitly chain together multiple tallies", {
  skip_on_cran()

  expect_snapshot({
    df <- data.frame(g = c(1, 1, 2, 2), n = 1:4)
    df$lat <- 1:4
    df$lon <- 1:4
    df <- terra::vect(df, crs = "EPSG:3857")

    df |> count(g, wt = n)
    df |>
      count(g, wt = n) |>
      count(wt = n)
    df |> count(n)
  })
})


# tally -------------------------------------------------------------------

test_that("tally can sort output", {
  skip_on_cran()

  gf <- group_by(data.frame(x = c(1, 1, 2, 2, 2)), x)
  gf$lon <- 1:5
  gf$lat <- 1:5
  gf <- as_spatvector(gf)
  expect_s4_class(gf, "SpatVector")
  expect_identical(group_vars(gf), "x")
  out <- tally(gf, sort = TRUE)
  expect_true(all(as_tibble(out) == data.frame(x = c(2, 1), n = c(3, 2))))
})

test_that("weighted tally drops NAs", {
  skip_on_cran()

  df <- tibble(x = c(1, 1, NA))
  df$lon <- 1:3
  df$lat <- df$lon
  df <- as_spatvector(df)
  expect_s4_class(df, "SpatVector")

  res <- tally(df, x)
  expect_s4_class(res, "SpatVector")
  expect_equal(res$n, 2)
})

test_that("tally() drops last group", {
  skip_on_cran()

  df <- data.frame(x = 1, y = 2, z = 3)
  df <- terra::vect(df, c("x", "y"), keepgeom = TRUE)

  res <- expect_message(df |> group_by(x, y) |> tally(), NA)
  expect_equal(group_vars(res), "x")
})

# SpatVector aggregation ------------------------------------------------------

test_that("count Check aggregation: POINTS", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v <- terra::centroids(v)
  v$gr <- rep_len(c("F", "B", "A", "B", "A"), 9)
  v$nn <- seq_len(nrow(v))

  expect_identical(terra::geomtype(v), "points")

  # Ungrouped
  # Dissolve
  v_ds <- count(v, .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = TRUE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Non dissolved
  v_ds <- count(v, .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Grouped
  # Dissolve
  v_ds <- count(v, gr, .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # No Dissolve
  v_ds <- count(v, gr, .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted Dissolved
  v_ds <- count(v, gr, .dissolve = TRUE, sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted Not Dissolved
  v_ds <- count(v, gr, .dissolve = FALSE, sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)
})

test_that("count Check aggregation: POLYGONS", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("F", "B", "A", "B", "A"), 9)
  v$nn <- seq_len(nrow(v))
  expect_identical(terra::geomtype(v), "polygons")

  # Ungrouped
  # Dissolve
  v_ds <- count(v, .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = TRUE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Non dissolved
  v_ds <- count(v, .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Grouped
  # Dissolve
  v_ds <- count(v, gr, .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # No Dissolve
  v_ds <- count(v, gr, .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted Dissolved
  v_ds <- count(v, gr, .dissolve = TRUE, sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted Not Dissolved
  v_ds <- count(v, gr, .dissolve = FALSE, sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)
})


test_that("count Check aggregation: LINES", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("F", "B", "A", "B", "A"), 9)
  v$nn <- seq_len(nrow(v))

  v <- terra::as.lines(v)

  expect_identical(terra::geomtype(v), "lines")

  # Ungrouped
  # Dissolve
  v_ds <- count(v, .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = TRUE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Non dissolved
  v_ds <- count(v, .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Grouped
  # Dissolve
  v_ds <- count(v, gr, .dissolve = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # No Dissolve
  v_ds <- count(v, gr, .dissolve = FALSE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted Dissolved
  v_ds <- count(v, gr, .dissolve = TRUE, sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = TRUE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted Not Dissolved
  v_ds <- count(v, gr, .dissolve = FALSE, sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)
})


test_that("tally Check aggregation: POINTS", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v <- terra::centroids(v)
  v$gr <- rep_len(c("F", "B", "A", "B", "A"), 9)
  v$nn <- seq_len(nrow(v))

  expect_identical(terra::geomtype(v), "points")

  # Ungrouped
  v_ds <- tally(v)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Grouped
  v_ds <- tally(group_by(v, gr))
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted
  v_ds <- tally(group_by(v, gr), sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)
})

test_that("tally Check aggregation: POLYGONS", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("F", "B", "A", "B", "A"), 9)
  v$nn <- seq_len(nrow(v))
  expect_identical(terra::geomtype(v), "polygons")

  # Ungrouped
  v_ds <- tally(v)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Grouped
  v_ds <- tally(group_by(v, gr))
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted
  v_ds <- tally(group_by(v, gr), sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)
})


test_that("tally Check aggregation: LINES", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("F", "B", "A", "B", "A"), 9)
  v$nn <- seq_len(nrow(v))

  v <- terra::as.lines(v)

  expect_identical(terra::geomtype(v), "lines")

  # Ungrouped
  v_ds <- tally(v)
  # Terra method
  t_ds <- terra::aggregate(v, dissolve = FALSE)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Grouped
  v_ds <- tally(group_by(v, gr))
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)

  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)

  # Sorted
  v_ds <- tally(group_by(v, gr), sort = TRUE)
  # Terra method
  t_ds <- terra::aggregate(v, "gr", dissolve = FALSE)
  t_ds <- t_ds[order(t_ds$agg_n, decreasing = TRUE), ]
  expect_equal(nrow(v_ds), 3)
  expect_equal(nrow(t_ds), 3)

  g_wkt <- terra::geom(v_ds, wkt = TRUE)
  t_wkt <- terra::geom(t_ds, wkt = TRUE)
  expect_identical(g_wkt, t_wkt)
})
