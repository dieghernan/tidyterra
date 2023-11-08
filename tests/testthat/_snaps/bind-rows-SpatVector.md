# bind_spat_rows respects rowwise

    Code
      gg <- bind_spat_rows(df2, df_init)
    Message
      ! Object 2 in `...` is <data.frame> 
      The result would present empty geoms

# bind_spat_rows respects named rowwise

    Code
      gg <- bind_spat_rows(df2, df_init)
    Message
      ! Object 2 in `...` is <data.frame> 
      The result would present empty geoms

# bind_spat_rows() give informative errors

    Code
      # invalid .id
      df1 <- data.frame(x = 1:3, lat = 1:3, lon = 1:3)
      df2 <- data.frame(x = 4:6, lat = 1:3, lon = 1:3)
      df1 <- terra::vect(df1)
      df2 <- terra::vect(df2)
      (expect_error(bind_spat_rows(df1, df2, .id = 5)))
    Output
      <error/rlang_error>
      Error in `dplyr::bind_rows()`:
      ! `.id` must be a single string, not the number 5.
    Code
      # invalid type
      ll <- list(data.frame(a = 1:5))
      (expect_error(bind_spat_rows(ll)))
    Output
      <error/rlang_error>
      Error in `bind_spat_rows()`:
      ! Object 1 in `...` is not a <SpatVector>
    Code
      (expect_error(bind_spat_rows(df1, ll)))
    Output
      <error/rlang_error>
      Error in `FUN()`:
      ! In `tidyterra::bind_spat_rows()`: object 2 in `...` is not a <data.frame>

# bind_spat_rows() give informative message

    Code
      # different crs SpatVector
      v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
      v1 <- v[1, ]
      v2 <- terra::project(v[2, ], "EPSG:3857")
      (expect_message(vend <- bind_spat_rows(v1, v2)))
    Output
      <cliMessage: ! Reprojecting object 2 in `...` since it  doesn't have the same CRS than object 1
      >
    Code
      expect_s4_class(vend, "SpatVector")
      expect_identical(pull_crs(vend), pull_crs(v1))
      # different crs sf
      v2_sf <- as_sf(v2)
      expect_s3_class(v2_sf, "sf")
      (expect_message(vend2 <- bind_spat_rows(v1, v2_sf)))
    Output
      <cliMessage: ! Reprojecting object 2 in `...` since it  doesn't have the same CRS than object 1
      >
    Code
      expect_s4_class(vend2, "SpatVector")
      expect_identical(pull_crs(vend2), pull_crs(v1))
      # different crs sf and df
      df1 <- data.frame(x = 1:3, lat = 1:3, lon = 1:3)
      (expect_message(vend3 <- bind_spat_rows(v1, v2_sf, df1)))
    Message
      ! Object 3 in `...` is <data.frame> 
      The result would present empty geoms
    Output
      <cliMessage: ! Reprojecting object 2 in `...` since it  doesn't have the same CRS than object 1
      >
    Code
      expect_s4_class(vend3, "SpatVector")
      expect_identical(pull_crs(vend3), pull_crs(v1))

