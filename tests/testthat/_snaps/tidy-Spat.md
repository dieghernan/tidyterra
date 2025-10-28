# Tidy SpatRasters pivot

    Code
      aa <- tidy(fort2, pivot = TRUE)
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::tidy.SpatRaster()`.
    Message
      ! Plotting only layer "tavg_04", "tavg_05", and "tavg_06" of class <numeric>

# Tidy SpatRasters pivot factor

    Code
      end <- check_mixed_cols(s_r_f_mix)
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message
      ! Plotting only layer "r1/r1", "r1/r2", "r2/r1", and "r2/r2" of class <factor>

