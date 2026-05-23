test_that("group_trim() drops empty groups", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$group <- factor(rep_len(c("A", "B"), nrow(v)), levels = c("A", "B", "C"))
  gv <- group_by(v, group, .drop = FALSE)

  trimmed <- group_trim(gv)

  expect_s4_class(trimmed, "SpatVector")
  expect_identical(n_groups(trimmed), 2L)
  expect_identical(group_vars(trimmed), "group")
  expect_identical(pull_crs(trimmed), pull_crs(v))
})
