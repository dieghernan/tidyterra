# Test errors

    Code
      inner_join(df1, df1)
    Condition
      Error in `error_spat_join()`:
      ! `y` must not be <SpatVector>. For spatial joins, use `terra::intersect()`.

---

    Code
      left_join(df1, df1)
    Condition
      Error in `error_spat_join()`:
      ! `y` must not be <SpatVector>. For spatial joins, use `terra::intersect()`.

---

    Code
      right_join(df1, df1)
    Condition
      Error in `error_spat_join()`:
      ! `y` must not be <SpatVector>. For spatial joins, use `terra::intersect()`.

---

    Code
      semi_join(df1, sf::st_as_sf(df1))
    Condition
      Error in `error_spat_join()`:
      ! `y` must not be <sf/data.frame>. For spatial joins, use `terra::intersect()`.

---

    Code
      anti_join(df1, df1)
    Condition
      Error in `error_spat_join()`:
      ! `y` must not be <SpatVector>. For spatial joins, use `terra::intersect()`.

---

    Code
      cross_join(df1, df1)
    Condition
      Error in `error_spat_join()`:
      ! `y` must not be <SpatVector>. For spatial joins, use `terra::intersect()`.

