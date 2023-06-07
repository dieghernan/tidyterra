# Name handling

    Code
      expect_message(vall <- distinct(v), "with duplicated/reserved")
    Message <cliMessage>
      ! Renaming columns:
    Output
      * `geometry` -> `geometry.1`
    Message <cliMessage>
      i Column(s) with duplicated/reserved names detected. See About layer/column
      names section on `tidyterra::as_tibble.SpatRaster()`
      ! Renaming columns:
    Output
      * `geometry` -> `geometry.1`

