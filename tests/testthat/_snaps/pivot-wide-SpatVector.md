# Remove geometry from values

    Code
      nc_unpivot <- pivot_wider(nc_pivoted, values_from = c(births, geometry),
      names_from = "year")
    Message
      ! Omitting "geometry" column from `values_from` argument.

# Remove geometry from names

    Code
      nc_unpivot <- pivot_wider(nc_pivoted, values_from = births, names_from = c(
        geometry, year))
    Message
      ! Omitting "geometry" column from `names_from` argument.

# error when overwriting existing column

    Code
      out <- pivot_wider(df, names_from = key, values_from = val, names_repair = "unique")
    Message
      New names:
      * `a` -> `a...2`
      * `a` -> `a...3`

# `names_from` must be supplied if `name` isn't in `data` (#1240)

    Code
      (expect_error(pivot_wider(df, values_from = val)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `pivot_wider()`:
      ! Can't select columns that don't exist.
      x Column `name` doesn't exist.

# Errors

    Code
      pv <- pivot_wider(df, names_from = key, values_from = val)
    Condition
      Error in `pivot_wider()`:
      ! Cannot rebuild the <SpatVector>. The "geometry" column was lost after pivoting.

