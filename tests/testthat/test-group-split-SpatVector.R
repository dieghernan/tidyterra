# group_split() -----------------------------------------------------------

test_that("group_split() returns SpatVector partitions", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  gv <- group_by(v, cpro)

  split_v <- group_split(gv)
  split_tbl <- dplyr::group_split(as_tibble(gv))

  expect_type(split_v, "list")
  expect_identical(class(split_v), "list")
  expect_length(split_v, length(split_tbl))
  expect_true(all(vapply(split_v, inherits, logical(1), "SpatVector")))
  expect_true(all(vapply(
    split_v,
    function(x) {
      !is_grouped_spatvector(x)
    },
    logical(1)
  )))
  split_v_tbl <- lapply(split_v, as_tibble)
  split_v_tbl <- lapply(split_v_tbl, function(x) {
    attr(x, "crs") <- NULL
    x
  })
  expect_identical(split_v_tbl, as.list(split_tbl))
  expect_equal(sum(vapply(split_v, nrow, double(1))), nrow(v))

  names(split_v) <- dplyr::pull(group_keys(gv), cpro)
  svc <- terra::svc(split_v)

  expect_s4_class(svc, "SpatVectorCollection")
  expect_length(svc, length(split_v))
  expect_named(svc, dplyr::pull(group_keys(gv), cpro))
})

test_that("group_split() supports temporary groups and .keep", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  split_v <- group_split(v, cpro, .keep = FALSE)
  split_tbl <- dplyr::group_split(as_tibble(v), cpro, .keep = FALSE)

  expect_length(split_v, length(split_tbl))
  split_v_tbl <- lapply(split_v, as_tibble)
  split_v_tbl <- lapply(split_v_tbl, function(x) {
    attr(x, "crs") <- NULL
    x
  })
  expect_identical(split_v_tbl, as.list(split_tbl))
  expect_false(any(vapply(
    split_v,
    function(x) {
      "cpro" %in% names(x)
    },
    logical(1)
  )))

  svc <- terra::svc(split_v)

  expect_s4_class(svc, "SpatVectorCollection")
  expect_length(svc, length(split_v))
})
