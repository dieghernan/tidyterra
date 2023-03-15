# From dplyr
# count -------------------------------------------------------------------

test_that("informs if n column already present, unless overridden", {
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

  df2 <- df1
  df2$nn <- 1:5

  expect_message(out <- count(df2, n), "already present")
  expect_s4_class(out, "SpatVector")
  expect_named(out, c("n", "nn"))

  expect_message(out <- count(df2, n, nn), "already present")
  expect_named(out, c("n", "nn", "nnn"))
})

test_that("name must be string", {
  df1 <- data.frame(x = c(1, 1, 2, 2, 2))
  df1$lon <- df1$x
  df1$lat <- df1$x
  df1 <- terra::vect(df1)

  expect_snapshot(error = TRUE, count(df1, x, name = 1))
  expect_snapshot(error = TRUE, count(df1, x, name = letters))
})

test_that("output preserves grouping", {
  df <- data.frame(g = c(1, 2, 2, 2))
  df$lon <- 1:4
  df$lat <- 1:4
  df <- terra::vect(df)
  exp <- data.frame(g = c(1, 2), n = c(1, 3))
  exp$lon <- 1:2
  exp$lat <- 1:2
  exp <- terra::vect(exp)
  debug(count.SpatVector)

  expect_equal(
    df %>% count(g) %>% as_tibble(),
    exp %>% as_tibble()
  )

  expect_equal(
    df %>% group_by(g) %>% count() %>% as_tibble(),
    exp %>% group_by(g) %>% as_tibble()
  )
})

test_that("output preserves class & attributes where possible", {
  df <- data.frame(g = c(1, 2, 2, 2))
  df$lon <- 1:4
  df$lat <- 1:4
  df <- terra::vect(df, crs = "EPSG:4326")

  out <- df %>% count(g)
  expect_s4_class(out, "SpatVector")
  expect_identical(group_vars(out), group_vars(df))
  expect_identical(pull_crs(out), pull_crs(df))

  out <- df %>%
    group_by(g) %>%
    count()
  expect_s4_class(out, "SpatVector")
  expect_equal(group_vars(out), "g")
  expect_identical(pull_crs(out), pull_crs(df))
})

test_that("can only explicitly chain together multiple tallies", {
  expect_snapshot({
    df <- data.frame(g = c(1, 1, 2, 2), n = 1:4)
    df$lat <- 1:4
    df$lon <- 1:4
    df <- terra::vect(df, crs = "EPSG:3857")

    df %>% count(g)
    df %>%
      count(g) %>%
      count()
    df %>% count(n)
  })
})


# tally -------------------------------------------------------------------

test_that("tally can sort output", {
  gf <- group_by(data.frame(x = c(1, 1, 2, 2, 2)), x)
  gf$lon <- 1:5
  gf$lat <- 1:5
  gf <- as_spatvector(gf)
  expect_s4_class(gf, "SpatVector")
  expect_identical(group_vars(gf), "x")
  out <- tally(gf, sort = TRUE)
  expect_true(all(as_tibble(out) == data.frame(x = c(2, 1), n = c(3, 2))))
})

test_that("tally() drops last group", {
  df <- data.frame(x = 1, y = 2, z = 3)
  df <- terra::vect(df, c("x", "y"), keepgeom = TRUE)

  res <- expect_message(df %>% group_by(x, y) %>% tally(), NA)
  expect_equal(group_vars(res), "x")
})

test_that("count Check aggregation: POINTS", {
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
