test_that("drop_na() preserves SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  r2 <- r
  r2[r == "Paleozoic"] <- NA
  d <- drop_na(r2)

  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  vdiffr::expect_doppelganger("dropna_01: original", autoplot(r))
  vdiffr::expect_doppelganger("dropna_02: dropped na", autoplot(d))
})

test_that("replace_na() preserves SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  r2 <- r
  r2[r == "Paleozoic"] <- NA
  d <- replace_na(r2, list(era = "Cenozoic"))

  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  vdiffr::expect_doppelganger("replacena_01: replaced na", autoplot(d))
})

test_that("select() preserves selected SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  r_plain <- local_constant_raster_layer(r, name = "aa", value = "20")

  d1 <- dplyr::select(c(r, r_plain), era)
  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  d2 <- dplyr::select(c(r_plain, r), era)
  expect_true(terra::has.colors(d2))
  expect_identical(terra::coltab(r), terra::coltab(d2))

  d3 <- dplyr::select(c(r_plain, r), aa, era)
  expect_equal(terra::has.colors(d3), c(FALSE, TRUE))
  expect_identical(terra::coltab(d3), c(list(NULL), terra::coltab(r)))

  d4 <- dplyr::select(c(r_plain, r), f = aa, era2 = era)
  expect_equal(terra::has.colors(d4), c(FALSE, TRUE))
  expect_identical(terra::coltab(d4), c(list(NULL), terra::coltab(r)))

  vdiffr::expect_doppelganger("select_01: first layer", autoplot(d1))
  vdiffr::expect_doppelganger("select_02: second layer", autoplot(d2))
  vdiffr::expect_doppelganger("select_03: several layers", autoplot(d3))
  vdiffr::expect_doppelganger(
    "select_04: several layers with rename",
    autoplot(d4)
  )
})

test_that("mutate() preserves existing SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  d1 <- dplyr::mutate(
    r,
    era = dplyr::if_else(era == "Paleozoic", "Cenozoic", era)
  )
  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  d2 <- dplyr::mutate(
    r,
    era_new = dplyr::case_when(
      era == "Cenozoic" ~ "Paleozoic",
      era == "Mesozoic" ~ "Cenozoic",
      TRUE ~ era
    )
  )
  expect_identical(terra::has.colors(d2), c(TRUE, FALSE))
  expect_identical(c(terra::coltab(r), list(NULL)), terra::coltab(d2))

  newctb <- local_letter_coltab_layer(r)
  several <- c(r, newctb)
  d3 <- several |> dplyr::mutate(another = "SAD")

  expect_identical(terra::has.colors(d3), c(TRUE, TRUE, FALSE))
  fullctab <- c(terra::coltab(r), terra::coltab(newctb), list(NULL))
  expect_identical(terra::coltab(d3), fullctab)

  d4 <- d3 |> dplyr::select(letter, another, era)
  expect_identical(terra::has.colors(d4), c(TRUE, FALSE, TRUE))
  expect_identical(terra::coltab(d4), fullctab[c(2, 3, 1)])

  vdiffr::expect_doppelganger("mutate_01: replace existing", autoplot(d1))
  vdiffr::expect_doppelganger("mutate_02: add plain layer", autoplot(d2))
  vdiffr::expect_doppelganger(
    "mutate_03: add plain layer to coltab stack",
    autoplot(d3)
  )
  vdiffr::expect_doppelganger("mutate_04: reorder coltab stack", autoplot(d4))
})

test_that("transmute() preserves retained SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  d1 <- dplyr::transmute(
    r,
    era = dplyr::case_when(
      era == "Cenozoic" ~ "Paleozoic",
      era == "Mesozoic" ~ "Cenozoic",
      TRUE ~ era
    )
  )
  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  d2 <- dplyr::transmute(
    r,
    era_new = dplyr::case_when(
      era == "Cenozoic" ~ "Paleozoic",
      era == "Mesozoic" ~ "Cenozoic",
      TRUE ~ era
    )
  )
  expect_false(terra::has.colors(d2))

  newctb <- local_letter_coltab_layer(r)
  several <- c(r, newctb)
  d3 <- several |> dplyr::transmute(letter = letter, era = era)
  expect_identical(terra::has.colors(d3), c(TRUE, TRUE))
  expect_identical(terra::coltab(d3), terra::coltab(several)[2:1])

  d4 <- dplyr::transmute(several, era2 = era, letter = letter, ss = "fcr")
  expect_identical(terra::has.colors(d4), c(FALSE, TRUE, FALSE))
  expect_identical(
    terra::coltab(d4),
    c(list(NULL), terra::coltab(newctb), list(NULL))
  )

  vdiffr::expect_doppelganger("transmute_01: replace existing", autoplot(d1))
  vdiffr::expect_doppelganger("transmute_02: create plain layer", autoplot(d2))
  vdiffr::expect_doppelganger(
    "transmute_03: reorder coltab stack",
    autoplot(d3)
  )
  vdiffr::expect_doppelganger(
    "transmute_04: mix retained and plain layers",
    autoplot(d4)
  )
})

test_that("filter() preserves SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  d <- dplyr::filter(r, era %in% c("Paleozoic", "Mesozoic"))
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  vdiffr::expect_doppelganger("filter_01: filtered values", autoplot(d))
})

test_that("slice helpers preserve SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  slice_cases <- list(
    dplyr::slice(r, 1:20),
    dplyr::slice_head(r, n = 50),
    dplyr::slice_tail(r, n = 50),
    dplyr::slice_min(r, era, n = 50),
    dplyr::slice_max(r, era, n = 50),
    dplyr::slice_sample(r, n = 50),
    slice_rows(r, 1:3),
    slice_cols(r, 1:3),
    slice_colrows(r, rows = 1:3, cols = 1:4)
  )

  for (sl in slice_cases) {
    expect_true(terra::has.colors(sl))
    expect_identical(terra::coltab(sl), terra::coltab(r))
  }
})

test_that("rename() preserves SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  d <- dplyr::rename(r, era_xxx = era)
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  vdiffr::expect_doppelganger("rename_01: renamed layer", autoplot(d))
})

test_that("relocate() preserves SpatRaster color tables", {
  r <- local_cyl_era_raster()
  expect_true(terra::has.colors(r))

  r_plain <- local_constant_raster_layer(r, name = "test", value = "A")
  d <- dplyr::relocate(c(r_plain, r), era, .before = "test")

  expect_identical(terra::has.colors(d), c(TRUE, FALSE))
  expect_identical(terra::coltab(d), c(terra::coltab(r), list(NULL)))

  vdiffr::expect_doppelganger("relocate_01: relocated layer", autoplot(d))
})
