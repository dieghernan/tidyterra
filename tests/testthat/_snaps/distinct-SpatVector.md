# Name handling

    Code
      expect_message(vall <- distinct(v), "with duplicated or reserved")
    Message
      ! Renaming columns and layers:
    Output
      * `geometry` -> `geometry.1`
    Message
      i Column(s) with duplicated or reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns and layers:
    Output
      * `geometry` -> `geometry.1`

