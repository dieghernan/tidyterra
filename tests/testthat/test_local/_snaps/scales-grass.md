# grass discrete colour scale maps palettes

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

# grass continuous colour scale maps palettes

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

# grass continuous colour scale maps palettes without GRASS range

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

# grass binned colour scale maps palettes

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

# grass binned colour scale maps palettes without GRASS range

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

# grass discrete fill scale maps palettes

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

# grass continuous fill scale maps palettes

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

# grass continuous fill scale maps palettes without GRASS range

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

# grass binned fill scale maps palettes

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

# grass binned fill scale maps palettes without GRASS range

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

# grass.colors() validates palette names and sizes

    Code
      grass.colors(20, "xx")
    Condition
      Error in `extract_pal()`:
      ! `palette` is not a known palette.

