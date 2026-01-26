test_that("SpatRaster mutate", {
  skip_on_cran()

  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  spatrast <- terra::rast(f)

  mod <- spatrast |>
    mutate(exp_lyr1 = exp(tavg_04 / 10))

  expect_true(compare_spatrasters(spatrast, mod))
  expect_gt(terra::nlyr(mod), terra::nlyr(spatrast))
})


test_that("SpatRaster mutate and check names", {
  skip_on_cran()

  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

  init <- terra::rast(f)

  spatrast <- init
  names(spatrast) <- c("a", "a", "b")

  expect_snapshot(fixed_names <- dplyr::mutate(spatrast, b2 = a + 100))

  all_eq <- all(
    names(fixed_names) ==
      make.names(
        c(
          names(spatrast),
          "b2"
        ),
        unique = TRUE
      )
  )

  expect_true(all_eq)

  expect_true(compare_spatrasters(init, fixed_names))

  # Same with layer names as x,y
  names(spatrast) <- c("x", "x", "y")
  expect_snapshot(
    fixed_names2 <- dplyr::mutate(spatrast, b2 = x.1 + x.2 + y.1)
  )

  all_eq <- all(
    names(fixed_names2) ==
      make.names(
        c(
          "x",
          "y",
          names(spatrast),
          "b2"
        ),
        unique = TRUE
      )[-c(1, 2)]
  )

  expect_true(all_eq)

  expect_true(compare_spatrasters(init, fixed_names2))
})

test_that("SpatVector", {
  skip_on_cran()

  # SpatVector method
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  mutated <- mutate(v, cpro2 = paste0(cpro, "-CyL"))

  expect_s4_class(mutated, "SpatVector")
  expect_gt(ncol(mutated), ncol(v))
  expect_identical(
    mutated$cpro2,
    paste0(v$cpro, "-CyL")
  )
})


test_that("mutate preserves grouping", {
  skip_on_cran()

  df <- tibble::tibble(x = 1:2, y = 2)
  df <- as_spatvector(df, geom = c("x", "y"), keepgeom = TRUE)
  gf <- group_by(df, x)

  out <- mutate(gf, x = 1)
  expect_equal(group_vars(out), "x")
  expect_equal(nrow(group_data(out)), 1)

  out <- mutate(gf, z = 1)
  expect_equal(group_data(out), group_data(gf))
})
# .by -------------------------------------------------------------------------

test_that("can group transiently using `.by`", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  out <- mutate(df, x = mean(x), .by = g)

  expect_identical(out$g, df$g)
  expect_identical(out$x, c(3, 3, 2, 3, 2))
  expect_s4_class(out, class(v))
})

test_that("transient grouping retains bare data.frame class", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- data.frame(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  out <- mutate(df, x = mean(x), .by = g)
  expect_s4_class(out, class(v))
})


test_that("can `NULL` out the `.by` column", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1:3)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  empty <- mutate(df, x = NULL, .by = x)
  expect_true(terra::ncol(empty) == 0)
})

test_that("catches `.by` with grouped-df", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  gdf <- group_by(df, x)

  expect_error(mutate(gdf, .by = x))
})

test_that("catches `.by` with rowwise-df", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  rdf <- rowwise(df)

  expect_error(mutate(rdf, .by = x))
})

# .before, .after, .keep ------------------------------------------------------

test_that(".keep = 'unused' keeps variables explicitly mentioned", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1, y = 2)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  out <- mutate(df, x1 = x + 1, y = y, .keep = "unused")
  expect_named(out, c("y", "x1"))
})

test_that(".keep = 'used' not affected by across() or pick()", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1, y = 2, z = 3, a = "a", b = "b", c = "c")
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  # This must evaluate every column in order to figure out if should
  # be included in the set or not, but that shouldn't be counted for
  # the purposes of "used" variables
  out <- mutate(
    df,
    dplyr::across(dplyr::where(is.numeric), identity),
    .keep = "unused"
  )
  expect_named(out, names(df))

  out <- mutate(df, dplyr::pick(dplyr::where(is.numeric)), .keep = "unused")
  expect_named(out, names(df))
})

test_that(".keep = 'used' keeps variables used in expressions", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- tibble(a = 1, b = 2, c = 3, x = 1, y = 2)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  out <- mutate(df, xy = x + y, .keep = "used")
  expect_named(out, c("x", "y", "xy"))
})

test_that(".keep = 'none' only keeps grouping variables", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- tibble(x = 1, y = 2)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  gf <- group_by(df, x)

  expect_named(mutate(df, z = 1, .keep = "none"), "z")
  expect_named(mutate(gf, z = 1, .keep = "none"), c("x", "z"))
})

test_that(".keep = 'none' retains original ordering (#5967)", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- tibble(x = 1, y = 2)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  expect_named(df |> mutate(y = 1, x = 2, .keep = "none"), c("x", "y"))

  # even when grouped
  gf <- group_by(df, x)
  expect_named(gf |> mutate(y = 1, x = 2, .keep = "none"), c("x", "y"))
})

test_that("can use .before and .after to control column position", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- tibble(x = 1, y = 2)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  expect_named(mutate(df, z = 1), c("x", "y", "z"))
  expect_named(mutate(df, z = 1, .before = 1), c("z", "x", "y"))
  expect_named(mutate(df, z = 1, .after = 1), c("x", "z", "y"))

  # but doesn't affect order of existing columns
  df <- tibble(x = 1, y = 2)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  expect_named(mutate(df, x = 1, .after = y), c("x", "y"))
})

test_that(".keep and .before/.after interact correctly", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1, y = 1, z = 1, a = 1, b = 2, c = 3)
  df <- cbind(v[seq_len(nrow(df)), 0], df) |> group_by(a, b)

  expect_named(mutate(df, d = 1, x = 2, .keep = "none"), c("x", "a", "b", "d"))
  expect_named(
    mutate(df, d = 1, x = 2, .keep = "none", .before = "a"),
    c("x", "d", "a", "b")
  )
  expect_named(
    mutate(df, d = 1, x = 2, .keep = "none", .after = "a"),
    c("x", "a", "d", "b")
  )
})

test_that("drop col with `NULL` then reading it retains original location", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1, y = 2, z = 3, a = 4)
  df <- cbind(v[seq_len(nrow(df)), 0], df)
  df <- group_by(df, z)

  expect_named(
    mutate(df, y = NULL, y = 3, .keep = "all"),
    c("x", "y", "z", "a")
  )
  expect_named(
    mutate(df, b = a, y = NULL, y = 3, .keep = "used"),
    c("y", "z", "a", "b")
  )
  expect_named(
    mutate(df, b = a, y = NULL, y = 3, .keep = "unused"),
    c("x", "y", "z", "b")
  )

  # It isn't treated as a "new" column
  expect_named(
    mutate(df, y = NULL, y = 3, .keep = "all", .before = x),
    c("x", "y", "z", "a")
  )
})

test_that("setting a new col to `NULL` works with `.before` and `.after`", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1, y = 2, z = 3, a = 4)
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  expect_named(mutate(df, b = NULL, .before = 1), names(df))
  expect_named(mutate(df, b = 1, b = NULL, .before = 1), names(df))
  expect_named(
    mutate(df, b = NULL, b = 1, .before = 1),
    c("b", "x", "y", "z", "a")
  )

  expect_named(
    mutate(df, b = NULL, c = 1, .after = 2),
    c("x", "y", "c", "z", "a")
  )
})

test_that(".keep= always retains grouping variables (#5582)", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  df <- tibble(x = 1, y = 2, z = 3)
  df <- cbind(v[seq_len(nrow(df)), 0], df) |>
    group_by(z)
  expect_named(
    df |> mutate(a = x + 1, .keep = "none"),
    c("z", "a")
  )
  expect_equal(
    df |> mutate(a = x + 1, .keep = "none") |> group_data(),
    tibble(x = 1, y = 2, z = 3, a = 2) |> group_by(z) |> group_data()
  )

  expect_equal(
    df |> mutate(a = x + 1, .keep = "all") |> group_data(),
    tibble(x = 1, y = 2, z = 3, a = 2) |> group_by(z) |> group_data()
  )
  expect_equal(
    df |> mutate(a = x + 1, .keep = "used") |> group_data(),
    tibble(x = 1, z = 3, a = 2) |> group_by(z) |> group_data()
  )
  expect_equal(
    df |> mutate(a = x + 1, .keep = "unused") |> group_data(),
    tibble(y = 2, z = 3, a = 2) |> group_by(z) |> group_data()
  )
})

# Raster.before, .after, .keep ------------------------------------------------------

test_that("rast .keep = 'unused' keeps variables explicitly mentioned", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)

  out <- spatrast |>
    mutate(x_n = 1, y_n = 2) |>
    select(x_n:y_n) |>
    mutate(x1 = x_n + 1, y_n = y_n, .keep = "unused")

  expect_named(out, c("y_n", "x1"))
})

test_that("rast .keep = 'used' not affected by across() or pick()", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)

  # This must evaluate every column in order to figure out if should
  # be included in the set or not, but that shouldn't be counted for
  # the purposes of "used" variables

  df <- spatrast |>
    mutate(x_n = 1, y_n = 2, z = 3, a = "a", b = "b", c = "c") |>
    select(x_n:c)

  out <- mutate(
    df,
    dplyr::across(dplyr::where(is.numeric), identity),
    .keep = "unused"
  )

  expect_named(out, names(df))

  out <- mutate(df, dplyr::pick(dplyr::where(is.numeric)), .keep = "unused")
  expect_named(out, names(df))
})

test_that("rast .keep = 'used' keeps variables used in expressions", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)
  df <- spatrast |>
    mutate(a = 1, b = 2, c = 3, x_n = 1, y_n = 2) |>
    select(a:y_n)

  out <- mutate(df, xy = x_n + y_n, .keep = "used")
  expect_named(out, c("x_n", "y_n", "xy"))
})

test_that("rast .keep = 'none' retains original ordering (#5967)", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)
  df <- spatrast |>
    mutate(x_n = 1, y_n = 2) |>
    select(-elevation_m)

  expect_named(df |> mutate(y_n = 1, x_n = 2, .keep = "none"), c("x_n", "y_n"))
})

test_that("rast can use .before and .after to control column position", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)
  df <- spatrast |>
    mutate(x_n = 1, y_n = 2) |>
    select(-elevation_m)

  expect_named(mutate(df, z = 1), c("x_n", "y_n", "z"))
  expect_named(mutate(df, z = 1, .before = 1), c("z", "x_n", "y_n"))
  expect_named(mutate(df, z = 1, .after = 1), c("x_n", "z", "y_n"))

  # but doesn't affect order of existing columns

  expect_named(mutate(df, x_n = 1, .after = y_n), c("x_n", "y_n"))
})

test_that("rast .keep and .before/.after interact correctly", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)
  df <- spatrast |>
    mutate(x_n = 1, y_n = 2, z = 1, a = 1, b = 2, c = 3) |>
    select(-elevation_m)

  expect_named(mutate(df, d = 1, x_n = 2, .keep = "none"), c("x_n", "d"))
  expect_named(
    mutate(df, d = 1, x_n = 2, .keep = "none", .before = "a"),
    c("x_n", "d")
  )
  expect_named(
    mutate(df, d = 1, x_n = 2, .keep = "none", .after = "a"),
    c("x_n", "d")
  )
})

test_that("rast drop col with `NULL` then reading it retains original loc", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)
  df <- spatrast |>
    mutate(x_n = 1, y_n = 2, z = 1, a = 1) |>
    select(-elevation_m)

  expect_named(
    mutate(df, y_n = NULL, y_n = 3, .keep = "all"),
    c("x_n", "y_n", "z", "a")
  )
  expect_named(
    mutate(df, b = a, y_n = NULL, y_n = 3, .keep = "used"),
    c("y_n", "a", "b")
  )
  expect_named(
    mutate(df, b = a, y_n = NULL, y_n = 3, .keep = "unused"),
    c("x_n", "y_n", "z", "b")
  )

  # It isn't treated as a "new" column
  expect_named(
    mutate(df, y_n = NULL, y_n = 3, .keep = "all", .before = x_n),
    c("x_n", "y_n", "z", "a")
  )
})

test_that("rast set a new col to `NULL` works with `.before` and `.after`", {
  skip_on_cran()
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  spatrast <- terra::rast(f)
  df <- spatrast |>
    mutate(x_n = 1, y_n = 2, z = 1, a = 1) |>
    select(-elevation_m)

  expect_named(mutate(df, b = NULL, .before = 1), names(df))
  expect_named(mutate(df, b = 1, b = NULL, .before = 1), names(df))
  expect_named(
    mutate(df, b = NULL, b = 1, .before = 1),
    c("b", "x_n", "y_n", "z", "a")
  )

  expect_named(
    mutate(df, b = NULL, c = 1, .after = 2),
    c("x_n", "y_n", "c", "z", "a")
  )
})
