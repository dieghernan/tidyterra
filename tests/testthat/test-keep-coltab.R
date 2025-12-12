# Test coltab is kept on SpatRaster methods
test_that("drop_na", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- r
  r2[r == "Paleozoic"] <- NA

  d <- drop_na(r2)
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))
})

test_that("replace_na", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- r
  r2[r == "Paleozoic"] <- NA

  d <- replace_na(r2, list(era = "Cenozoic"))
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))
})

# Dplyr methods

test_that("select", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- r
  terra::values(r2) <- "20"
  names(r2) <- "aa"
  # In first place
  rend <- c(r, r2)
  d1 <- select(rend, era)

  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  # In second place
  rend <- c(r2, r)
  d2 <- select(rend, era)

  expect_true(terra::has.colors(d2))
  expect_identical(terra::coltab(r), terra::coltab(d2))

  # Selecting severals
  d3 <- select(rend, aa, era)

  expect_equal(terra::has.colors(d3), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d3),
    c(list(NULL), terra::coltab(r))
  )

  # Selecting severals with rename
  d4 <- select(rend, f = aa, era2 = era)

  expect_equal(terra::has.colors(d4), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d4),
    c(list(NULL), terra::coltab(r))
  )
})

test_that("mutate", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))

  # No adding new cols
  d1 <- mutate(r, era = dplyr::if_else(era == "Paleozoic", "Cenozoic", era))

  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  # Adding a new col
  d2 <- mutate(
    r,
    era_new = dplyr::case_when(
      era == "Cenozoic" ~ "Paleozoic",
      era == "Mesozoic" ~ "Cenozoic",
      TRUE ~ era
    )
  )

  expect_identical(terra::has.colors(d2), c(TRUE, FALSE))
  expect_identical(c(terra::coltab(r), list(NULL)), terra::coltab(d2))

  # Adding a new layer with different coltab
  newctb <- terra::rast(r)
  names(newctb) <- "newctb"
  terra::values(newctb) <- as.factor(rep_len(
    c("S", "W", "S"),
    terra::ncell(newctb)
  ))
  levels(newctb) <- data.frame(id = 1:2, letter = c("S", "W"))
  coltb2 <- data.frame(
    value = 1:2,
    t(col2rgb(c("red", "yellow"), alpha = TRUE))
  )
  terra::coltab(newctb) <- coltb2
  several <- c(r, newctb)
  d3 <- several |> mutate(another = "SAD")

  expect_identical(terra::has.colors(d3), c(TRUE, TRUE, FALSE))

  fullctab <- c(terra::coltab(r), terra::coltab(newctb), list(NULL))
  expect_identical(terra::coltab(d3), fullctab)

  # Additional test select
  d4 <- d3 |> select(letter, another, era)
  expect_identical(terra::has.colors(d4), c(TRUE, FALSE, TRUE))
  expect_identical(terra::coltab(d4), fullctab[c(2, 3, 1)])
})

test_that("transmute", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))

  # transmute
  d1 <- transmute(
    r,
    era = dplyr::case_when(
      era == "Cenozoic" ~ "Paleozoic",
      era == "Mesozoic" ~ "Cenozoic",
      TRUE ~ era
    )
  )

  expect_identical(terra::has.colors(d1), TRUE)
  expect_identical(terra::coltab(r), terra::coltab(d1))

  # transmute a new var with no coltab
  d2 <- transmute(
    r,
    era_new = dplyr::case_when(
      era == "Cenozoic" ~ "Paleozoic",
      era == "Mesozoic" ~ "Cenozoic",
      TRUE ~ era
    )
  )

  expect_identical(terra::has.colors(d2), FALSE)

  # Adding a new layer with different coltab
  newctb <- terra::rast(r)
  names(newctb) <- "newctb"
  terra::values(newctb) <- as.factor(rep_len(
    c("S", "W", "S"),
    terra::ncell(newctb)
  ))
  levels(newctb) <- data.frame(id = 1:2, letter = c("S", "W"))
  coltb2 <- data.frame(
    value = 1:2,
    t(col2rgb(c("red", "yellow"), alpha = TRUE))
  )
  terra::coltab(newctb) <- coltb2
  several <- c(r, newctb)
  d3 <- several |> transmute(letter = letter, era = era)

  expect_identical(terra::has.colors(d3), c(TRUE, TRUE))
  expect_identical(terra::coltab(d3), terra::coltab(several)[2:1])

  # Mix and match

  d4 <- transmute(several, era2 = era, letter = letter, ss = "fcr")

  expect_identical(terra::has.colors(d4), c(FALSE, TRUE, FALSE))
  expect_identical(
    terra::coltab(d4),
    c(
      list(NULL),
      terra::coltab(newctb),
      list(NULL)
    )
  )
})

test_that("filter", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  d <- filter(r, era %in% c("Paleozoic", "Mesozoic"))
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))
})

test_that("slice", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))

  # Slice
  sl <- slice(r, 1:20)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice head
  sl <- slice_head(r, n = 50)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice tail
  sl <- slice_tail(r, n = 50)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice min
  sl <- slice_min(r, era, n = 50)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice max
  sl <- slice_max(r, era, n = 50)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice sample
  sl <- slice_sample(r, n = 50)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice rows
  sl <- slice_rows(r, 1:3)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice cols
  sl <- slice_cols(r, 1:3)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))

  # Slice rowcols
  sl <- slice_colrows(r, rows = 1:3, cols = 1:4)
  expect_true(terra::has.colors(sl))
  expect_identical(terra::coltab(sl), terra::coltab(r))
})


test_that("rename", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  d <- rename(r, era_xxx = era)
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))
})

test_that("relocate", {
  skip_on_cran()
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  # New raster
  r2 <- terra::rast(r)
  terra::values(r2) <- rep_len("A", terra::ncell(r))
  names(r2) <- "test"
  rend <- c(r2, r)
  d <- relocate(rend, era, .before = "test")
  expect_identical(terra::has.colors(d), c(TRUE, FALSE))
  expect_identical(terra::coltab(d), c(terra::coltab(r), list(NULL)))
})
