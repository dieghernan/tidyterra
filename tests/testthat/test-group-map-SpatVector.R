test_that("group_map() passes SpatVector groups and tibble keys", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$grp <- rep(c("a", "b"), length.out = nrow(v))

  out <- v |>
    group_by(grp) |>
    group_map(
      ~ {
        expect_s4_class(.x, "SpatVector")
        expect_s3_class(.y, "tbl_df")
        mutate(.x, key = .y$grp)
      }
    )

  expect_length(out, 2)
  expect_true(all(vapply(out, inherits, logical(1), "SpatVector")))
})

test_that("group_modify() binds SpatVector group results", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$grp <- rep(c("a", "b"), length.out = nrow(v))

  expect_snapshot(
    error = TRUE,
    v |>
      group_modify(~ head(.x, 0))
  )

  out <- v |>
    group_by(grp) |>
    group_modify(~ mutate(.x, key = .y$grp))

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), nrow(v))
  expect_true("key" %in% names(out))
})

test_that("Split keys", {
  skip_on_cran()
  expect_identical(
    split_keys(tibble::tibble()),
    list(tibble::tibble())
  )
  expect_length(
    split_keys(tibble::tibble(a = letters[1:3])),
    3
  )
})
