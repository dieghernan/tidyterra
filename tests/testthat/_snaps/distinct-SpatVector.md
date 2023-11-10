# Name handling

    Code
      expect_message(vall <- distinct(v), "with duplicated/reserved")
    Message
      ! Renaming columns:
    Output
      * `geometry` -> `geometry.1`
    Message
      i Column(s) with duplicated/reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns:
    Output
      * `geometry` -> `geometry.1`

