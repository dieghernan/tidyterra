# Discrete scale

    Code
      p + scale_colour_grass_d(alpha = -1)
    Condition
      Error in `scale_colour_grass_d()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_grass_d(direction = 0.5)
    Condition
      Error in `scale_colour_grass_d()`:
      ! `direction` must be either 1 or -1.

# Continous scale

    Code
      p + scale_colour_grass_c(alpha = -1)
    Condition
      Error in `scale_colour_grass_c()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_grass_c(direction = 0.5)
    Condition
      Error in `scale_colour_grass_c()`:
      ! `direction` must be either 1 or -1.

# Continous scale no range

    Code
      p + scale_colour_grass_c(palette = "x", use_grass_range = FALSE)
    Condition
      Error in `scale_colour_grass_c()`:
      ! `palette` "x" is not a known palette. See `?tidyterra::grass_db()`.

---

    Code
      p + scale_colour_grass_c(alpha = -1)
    Condition
      Error in `scale_colour_grass_c()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_grass_c(direction = -12)
    Condition
      Error in `scale_colour_grass_c()`:
      ! `direction` must be either 1 or -1.

# Breaking scale

    Code
      p_init + scale_color_grass_b(alpha = -1)
    Condition
      Error in `scale_color_grass_b()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_grass_b(direction = 0.5)
    Condition
      Error in `scale_colour_grass_b()`:
      ! `direction` must be either 1 or -1.

# Breaking scale no range

    Code
      p + scale_colour_grass_b(palette = "x")
    Condition
      Error in `scale_colour_grass_b()`:
      ! `palette` "x" is not a known palette. See `?tidyterra::grass_db()`.

---

    Code
      p + scale_colour_grass_b(alpha = -1)
    Condition
      Error in `scale_colour_grass_b()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_grass_b(direction = -12)
    Condition
      Error in `scale_colour_grass_b()`:
      ! `direction` must be either 1 or -1.

# Discrete scale fill

    Code
      p + scale_fill_grass_d(alpha = -1)
    Condition
      Error in `scale_fill_grass_d()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_grass_d(direction = 0.5)
    Condition
      Error in `scale_fill_grass_d()`:
      ! `direction` must be either 1 or -1.

# Continous scale fill

    Code
      p + scale_fill_grass_c(alpha = -1)
    Condition
      Error in `scale_fill_grass_c()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_grass_c(direction = 0.5)
    Condition
      Error in `scale_fill_grass_c()`:
      ! `direction` must be either 1 or -1.

# Continous scale fill no range

    Code
      p + scale_fill_grass_c(palette = "x")
    Condition
      Error in `scale_fill_grass_c()`:
      ! `palette` "x" is not a known palette. See `?tidyterra::grass_db()`.

---

    Code
      p + scale_fill_grass_c(alpha = -1)
    Condition
      Error in `scale_fill_grass_c()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_grass_c(direction = -12)
    Condition
      Error in `scale_fill_grass_c()`:
      ! `direction` must be either 1 or -1.

# Breaking scale fill

    Code
      p_init + scale_fill_grass_b(alpha = -1)
    Condition
      Error in `scale_fill_grass_b()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_grass_b(direction = 0.5)
    Condition
      Error in `scale_fill_grass_b()`:
      ! `direction` must be either 1 or -1.

# Breaking scale fill no range

    Code
      p + scale_fill_grass_b(palette = "x")
    Condition
      Error in `scale_fill_grass_b()`:
      ! `palette` "x" is not a known palette. See `?tidyterra::grass_db()`.

---

    Code
      p + scale_fill_grass_b(alpha = -1)
    Condition
      Error in `scale_fill_grass_b()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_grass_b(direction = -12)
    Condition
      Error in `scale_fill_grass_b()`:
      ! `direction` must be either 1 or -1.

# Palettes

    Code
      grass.colors(20, "xx")
    Condition
      Error in `extract_pal()`:
      ! `palette` is not a known palette.

# PR 165

    Code
      data$.label
    Output
      [1] "-10000" "-5000"  "0"      "5000"  

---

    Code
      data$.label
    Output
      [1] "-7500" "-5000" "-2500" "0"    

---

    Code
      data$.label
    Output
      [1] "0"    "500"  "1000" "1500" "2000"

