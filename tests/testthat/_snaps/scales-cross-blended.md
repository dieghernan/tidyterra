# Discrete scale

    Code
      p + scale_fill_cross_blended_d(alpha = -1)
    Condition
      Error in `scale_fill_cross_blended_d()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_d(direction = 0.5)
    Condition
      Error in `scale_fill_cross_blended_d()`:
      ! `direction` must be 1 or -1

# Discrete scale tint

    Code
      ggplot2::ggplot_build(perr)
    Condition
      Error in `extract_pal()`:
      ! `palette` does not match any given palette

---

    Code
      p + scale_fill_cross_blended_tint_d(alpha = -1)
    Condition
      Error in `scale_fill_cross_blended_tint_d()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_tint_d(direction = 0.5)
    Condition
      Error in `scale_fill_cross_blended_tint_d()`:
      ! `direction` must be 1 or -1

# Continous scale

    Code
      p + scale_fill_cross_blended_c(alpha = -1)
    Condition
      Error in `scale_fill_cross_blended_c()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_c(direction = 0.5)
    Condition
      Error in `scale_fill_cross_blended_c()`:
      ! `direction` must be 1 or -1

# Continous scale tint

    Code
      perr <- p + scale_fill_cross_blended_tint_c(palette = "aa")
    Condition
      Error in `scale_fill_cross_blended_tint_c()`:
      ! `palette` "palette" does not match any given palette. See `?tidyterra::cross_blended_hypsometric_tints_db()`

---

    Code
      p + scale_fill_cross_blended_tint_c(alpha = -1)
    Condition
      Error in `scale_fill_cross_blended_tint_c()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_tint_c(direction = 0.5)
    Condition
      Error in `scale_fill_cross_blended_tint_c()`:
      ! `direction` must be 1 or -1

# Breaking scale

    Code
      p_init + scale_fill_cross_blended_b(alpha = -1)
    Condition
      Error in `scale_fill_cross_blended_b()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_b(direction = 0.5)
    Condition
      Error in `scale_fill_cross_blended_b()`:
      ! `direction` must be 1 or -1

# Breaking scale tint

    Code
      perr <- p + scale_fill_cross_blended_tint_b(palette = "aa")
    Condition
      Error in `scale_fill_cross_blended_tint_b()`:
      ! `palette` "palette" does not match any given palette. See `?tidyterra::cross_blended_hypsometric_tints_db()`

---

    Code
      p_init + scale_fill_cross_blended_tint_b(alpha = -1)
    Condition
      Error in `scale_fill_cross_blended_tint_b()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_tint_b(direction = 0.5)
    Condition
      Error in `scale_fill_cross_blended_tint_b()`:
      ! `direction` must be 1 or -1

# Palette

    Code
      cross_blended.colors(20, "xx")
    Condition
      Error in `extract_pal()`:
      ! `palette` does not match any given palette

# Palette2

    Code
      cross_blended.colors2(20, "xx")
    Condition
      Error in `extract_pal()`:
      ! `palette` does not match any given palette

# Discrete scale col

    Code
      p + scale_colour_cross_blended_d(alpha = -1)
    Condition
      Error in `scale_colour_cross_blended_d()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_cross_blended_d(direction = 0.5)
    Condition
      Error in `scale_colour_cross_blended_d()`:
      ! `direction` must be 1 or -1

# Discrete scale col tint

    Code
      ggplot2::ggplot_build(perr)
    Condition
      Error in `extract_pal()`:
      ! `palette` does not match any given palette

---

    Code
      p + scale_colour_cross_blended_tint_d(alpha = -1)
    Condition
      Error in `scale_colour_cross_blended_tint_d()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_cross_blended_tint_d(direction = 0.5)
    Condition
      Error in `scale_colour_cross_blended_tint_d()`:
      ! `direction` must be 1 or -1

# Continous scale col

    Code
      p + scale_colour_cross_blended_c(alpha = -1)
    Condition
      Error in `scale_colour_cross_blended_c()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_cross_blended_c(direction = 0.5)
    Condition
      Error in `scale_colour_cross_blended_c()`:
      ! `direction` must be 1 or -1

# Continous scale col tint

    Code
      perr <- p + scale_color_cross_blended_tint_c(palette = "aa")
    Condition
      Error in `scale_color_cross_blended_tint_c()`:
      ! `palette` "palette" does not match any given palette. See `?tidyterra::cross_blended_hypsometric_tints_db()`

---

    Code
      p + scale_colour_cross_blended_tint_c(alpha = -1)
    Condition
      Error in `scale_colour_cross_blended_tint_c()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_cross_blended_tint_c(direction = 0.5)
    Condition
      Error in `scale_colour_cross_blended_tint_c()`:
      ! `direction` must be 1 or -1

# Breaking scale col

    Code
      p_init + scale_colour_cross_blended_b(alpha = -1)
    Condition
      Error in `scale_colour_cross_blended_b()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_cross_blended_b(direction = 0.5)
    Condition
      Error in `scale_colour_cross_blended_b()`:
      ! `direction` must be 1 or -1

# Breaking scale col tint

    Code
      perr <- p + scale_colour_cross_blended_tint_b(palette = "aa")
    Condition
      Error in `scale_colour_cross_blended_tint_b()`:
      ! `palette` "palette" does not match any given palette. See `?tidyterra::cross_blended_hypsometric_tints_db()`

---

    Code
      p_init + scale_colour_cross_blended_tint_b(alpha = -1)
    Condition
      Error in `scale_colour_cross_blended_tint_b()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_cross_blended_tint_b(direction = 0.5)
    Condition
      Error in `scale_colour_cross_blended_tint_b()`:
      ! `direction` must be 1 or -1

