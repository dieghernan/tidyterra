# Test errors

    Code
      inner_join(df1, df1)
    Error <rlang_error>
      `y` should not be <SpatVector>. For spatial joins use `terra::intersect()`

---

    Code
      left_join(df1, df1)
    Error <rlang_error>
      `y` should not be <SpatVector>. For spatial joins use `terra::intersect()`

---

    Code
      right_join(df1, df1)
    Error <rlang_error>
      `y` should not be <SpatVector>. For spatial joins use `terra::intersect()`

---

    Code
      semi_join(df1, sf::st_as_sf(df1))
    Error <rlang_error>
      `y` should not be <sf/data.frame>. For spatial joins use `terra::intersect()`

---

    Code
      anti_join(df1, df1)
    Error <rlang_error>
      `y` should not be <SpatVector>. For spatial joins use `terra::intersect()`

