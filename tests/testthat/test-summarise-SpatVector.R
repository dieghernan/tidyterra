test_that("Summarise gives the same results than default method", {
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nogroup <- summarise(v,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74)
  )
  nogroup_df <- summarise(as_tibble(v),
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74)
  )


  expect_s4_class(nogroup, "SpatVector")
  expect_s3_class(nogroup_df, "tbl")
  expect_true(all(nogroup_df == as_tibble(nogroup)))

  g <- group_by(v, SID74, SID79)
  g_df <- group_by(as_tibble(v), SID74, SID79)

  g_summ <- summarise(g,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74)
  )

  g_summ_df <- summarise(g_df,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74)
  )

  expect_s4_class(g_summ, "SpatVector")
  expect_s3_class(g_summ_df, "tbl")
  expect_true(all(g_summ_df == as_tibble(g_summ)))
})

test_that("Summarise preserve CRS", {
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  nogroup <- summarise(v,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74)
  )
  expect_s4_class(nogroup, "SpatVector")
  expect_identical(pull_crs(v), pull_crs(nogroup))
})

test_that("Summarise handles dissolve", {
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  diss <- summarise(v,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74), .dissolve = TRUE
  )
  expect_s4_class(diss, "SpatVector")

  dissolved_pols <- terra::disagg(diss)


  nodiss <- summarise(v,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74), .dissolve = FALSE
  )
  expect_s4_class(nodiss, "SpatVector")

  nodissolved_pols <- terra::disagg(nodiss)

  # Same statistics
  expect_identical(as_tibble(diss), as_tibble(nodiss))

  expect_gt(nrow(nodissolved_pols), nrow(dissolved_pols))
})


test_that("Summarise handles dissolve on groupds", {
  v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

  v_g <- group_by(v, SID74, SID79)
  diss <- summarise(v_g,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74), .dissolve = TRUE
  )
  expect_s4_class(diss, "SpatVector")

  dissolved_pols <- terra::disagg(diss)


  nodiss <- summarise(v_g,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74), .dissolve = FALSE
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

  diss_df <- summarise(df,
    sum_all = sum(AREA), n_all = dplyr::n(),
    mean = mean(BIR74)
  )

  expect_s3_class(diss_df, "tbl")
  expect_true(all(diss_df == as_tibble(diss)))
  expect_true(all(diss_df == as_tibble(nodiss)))

  expect_true(dplyr::is_grouped_df(diss_df))
  expect_identical(group_data(diss_df), group_data(diss))
  expect_identical(group_data(diss_df), group_data(nodiss))
})
