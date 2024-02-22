# can handle missing combinations

    Code
      pv <- pivot_longer(df, -id, names_to = c(".value", "n"), names_sep = "_")
    Message
      ! Ommiting "geometry" column from `cols` argument.

# original col order is preserved

    Code
      pv <- pivot_longer(df, -id, names_to = c(".value", "n"), names_sep = "_")
    Message
      ! Ommiting "geometry" column from `cols` argument.

# can pivot duplicated names to .value

    Code
      pv1 <- pivot_longer(df, -x, names_to = c(".value", NA), names_sep = "_")
    Message
      ! Ommiting "geometry" column from `cols` argument.

# Check tidyselect: var1:var10

    Code
      out <- remove_geom_col(tbl, a:char, "test_that")
    Message
      ! Ommiting "geometry" column from `test_that` argument.

# Check tidyselect: start_with

    Code
      out <- remove_geom_col(tbl, dplyr::starts_with("g"), "test_that")
    Message
      ! Ommiting "geometry" column from `test_that` argument.

# Check tidyselect: ends_with

    Code
      out <- remove_geom_col(tbl, dplyr::ends_with("y"), "test_that")
    Message
      ! Ommiting "geometry" column from `test_that` argument.

---

    Code
      out <- remove_geom_col(tbl, dplyr::ends_with("y"), "test_that")
    Message
      ! Ommiting "geometry" column from `test_that` argument.

# Check tidyselect: whereis

    Code
      out <- remove_geom_col(tbl, dplyr::where(is.character), "test_that")
    Message
      ! Ommiting "geometry" column from `test_that` argument.

