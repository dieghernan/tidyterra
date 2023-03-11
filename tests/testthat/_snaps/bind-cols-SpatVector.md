# bind_spat_cols() repairs names

    Code
      bound <- bind_spat_cols(df, df)
    Message <rlib_message_name_repair>
      New names:
      * `a` -> `a...1`
      * `b` -> `b...2`
      * `a` -> `a...4`
      * `b` -> `b...5`

# bind_spat_cols() gives informative errors

    Code
      # # incompatible size
      (expect_error(bind_spat_cols(mtcars)))
    Output
      <error/rlang_error>
      Error in `bind_spat_cols()`:
      ! Object #1 in ... is not a SpatVector

