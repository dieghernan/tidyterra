# bind_spat_rows respects rowwise

    Code
      gg <- bind_spat_rows(df2, df_init)
    Message
      ! Object 2 in `...` is <data.frame> 
      The result includes empty geometries.

# bind_spat_rows respects named rowwise

    Code
      gg <- bind_spat_rows(df2, df_init)
    Message
      ! Object 2 in `...` is <data.frame> 
      The result includes empty geometries.

# bind_spat_rows() errors on geometry-only SpatVectors

    Code
      bind_spat_rows(v, v)
    Condition
      Error:
      ! [cbind] nrow does not match

# bind_spat_rows() give informative errors

    Code
      bind_spat_rows(df1, df2, .id = 5)
    Condition
      Error in `dplyr::bind_rows()`:
      ! `.id` must be a single string, not the number 5.

---

    Code
      bind_spat_rows(ll)
    Condition
      Error in `bind_spat_rows()`:
      ! Object 1 in `...` is not a <SpatVector>.

---

    Code
      bind_spat_rows(df1, ll)
    Condition
      Error in `FUN()`:
      ! In `tidyterra::bind_spat_rows()`: object 2 in `...` is not a <data.frame>.

