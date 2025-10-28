test_that("Required packages", {
  file_path <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(file_path)

  expect_identical(required_pkgs(r), "terra")

  #  With vectors
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  expect_identical(required_pkgs(v), "terra")

  expect_identical(required_pkgs(terra::ext(r)), "terra")

  expect_identical(required_pkgs(terra::graticule()), "terra")
})
