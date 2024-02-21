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

