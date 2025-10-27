test_that("Row-wise are kept with project", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("B", "A", "C", "B"), nrow(v))

  gr_v <- rowwise(v, gr)

  expect_true(is_rowwise_spatvector(gr_v))

  gr_v2 <- gr_v %>% terra::project("EPSG:4326")

  expect_true(is_rowwise_spatvector(gr_v2))

  expect_identical(group_data(gr_v), group_data(gr_v2))
})

test_that("Row-wise are kept with casting", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("B", "A", "C", "B"), nrow(v))

  gr_v <- rowwise(v, gr)

  expect_true(is_rowwise_spatvector(gr_v))

  gr_v2 <- gr_v %>% terra::centroids()

  expect_true(is_rowwise_spatvector(gr_v2))

  expect_identical(group_data(gr_v), group_data(gr_v2))
})


test_that("Aggregate can re-rowwise", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("B", "A", "C", "B"), nrow(v))

  gr_v <- rowwise(v, gr) %>% terra::centroids()

  expect_true(is_rowwise_spatvector(gr_v))

  gr_v2 <- terra::aggregate(gr_v, by = "gr", count = TRUE)
  expect_true(is_rowwise_spatvector(gr_v2))

  # Trigger rebuild with any verb
  gr_v2 <- gr_v2 %>% mutate(a2 = 1)

  expect_identical(group_indices(gr_v2), c(1L, 2L, 3L))
})


test_that("Slicing can re-rowwise", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("B", "A", "C", "B"), nrow(v))

  gr_v <- rowwise(v, gr) %>% terra::centroids()

  expect_true(is_rowwise_spatvector(gr_v))

  gr_v2 <- gr_v[c(1:3, 7:9), ]
  expect_true(is_rowwise_spatvector(gr_v2))

  # Trigger rebuild with any verb
  gr_v2 <- gr_v2 %>% mutate(a = 1)

  # Same as
  gr_v_tbl <- as_tibble(gr_v)[c(1:3, 7:9), ]

  expect_identical(group_data(gr_v2), group_data(gr_v_tbl))
})

test_that("SpatSample does not re-rowwise", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("B", "A", "C", "B"), nrow(v))

  gr_v <- rowwise(v, gr)

  expect_true(is_rowwise_spatvector(gr_v))

  gr_v2 <- terra::spatSample(gr_v, 20)
  expect_identical(nrow(gr_v2), 20)

  expect_false(is_rowwise_spatvector(gr_v2))
})

test_that("Subset columns can re-rowwise", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("B", "A", "C", "B"), nrow(v))
  v$gr2 <- rep_len(c("F", "E"), nrow(v))
  gr_v <- rowwise(v, gr2, gr) %>% terra::centroids()

  expect_true(is_rowwise_spatvector(gr_v))

  expect_identical(group_vars(gr_v), c("gr2", "gr"))
  gr_v2 <- gr_v[, c("iso2", "gr")]
  expect_true(is_rowwise_spatvector(gr_v2))

  # Trigger rebuild with any verb
  gr_v2 <- gr_v2 %>% mutate(a = 1)
  expect_identical(group_vars(gr_v2), "gr")

  # Same as
  gr_v_tbl <- as_tibble(gr_v)[, c("iso2", "gr")]

  expect_identical(group_data(gr_v2), group_data(gr_v_tbl))
})

test_that("Subset all columns ungroup", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$gr <- rep_len(c("B", "A", "C", "B"), nrow(v))
  v$gr2 <- rep_len(c("F", "E"), nrow(v))
  gr_v <- rowwise(v, gr2, gr) %>% terra::centroids()

  expect_true(is_rowwise_spatvector(gr_v))

  expect_identical(group_vars(gr_v), c("gr2", "gr"))
  gr_v2 <- gr_v[, "iso2"]

  # Trigger rebuild with any verb
  expect_message(gr_v2 <- gr_v2 %>% mutate(a = 1), "mixed terra and tidyterra")
  expect_false(is_rowwise_spatvector(gr_v2))
  expect_identical(group_vars(gr_v2), character(0))

  # Same as
  gr_v_tbl <- as_tibble(v)[, "iso2"]

  expect_identical(group_data(gr_v2), group_data(gr_v_tbl))
})

test_that("Gives meaningful messages", {
  skip_on_cran()
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  gr_v <- rowwise(v, iso2) %>% terra::centroids()

  expect_true(is_rowwise_spatvector(gr_v))

  gr_v2 <- gr_v[, "name"]
  expect_snapshot(gr_v2 <- gr_v2 %>% mutate(a = 1))
})
