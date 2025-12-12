test_that("multiplication works", {
  cyl <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  pl <- ggplot2::ggplot(cyl) +
    stat_spat_coordinates()
  expect_s3_class(pl, "ggplot")
})
