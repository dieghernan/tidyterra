# Discrete scale

    Code
      p + scale_fill_cross_blended_d(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_d(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Discrete scale tint

    Code
      ggplot2::ggplot_build(s)
    Error <rlang_error>
      `palette` does not match any given palette

---

    Code
      p + scale_fill_cross_blended_tint_d(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_tint_d(direction = -12)
    Error <rlang_error>
      `direction` must be 1 or -1

# Continous scale

    Code
      p + scale_fill_cross_blended_c(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_c(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Continous scale tint

    Code
      p + scale_fill_cross_blended_tint_c(palette = "x")
    Error <rlang_error>
      `palette` "palette" does not match any given palette. See `?tidyterra::cross_blended_hypsometric_tints_db()`

---

    Code
      p + scale_fill_cross_blended_tint_c(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_tint_c(direction = -12)
    Error <rlang_error>
      `direction` must be 1 or -1

# Breaking scale

    Code
      p_init + scale_fill_cross_blended_b(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_b(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Breaking scale tint

    Code
      p + scale_fill_cross_blended_tint_b(palette = "x")
    Error <rlang_error>
      `palette` "palette" does not match any given palette. See `?tidyterra::cross_blended_hypsometric_tints_db()`

---

    Code
      p + scale_fill_cross_blended_tint_b(palette = "x")
    Error <rlang_error>
      `palette` "palette" does not match any given palette. See `?tidyterra::cross_blended_hypsometric_tints_db()`

---

    Code
      p + scale_fill_cross_blended_tint_b(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_cross_blended_tint_b(direction = -12)
    Error <rlang_error>
      `direction` must be 1 or -1

# Palettes

    Code
      cross_blended.colors(20, "xx")
    Error <rlang_error>
      `palette` does not match any given palette

# Palettes2

    Code
      cross_blended.colors2(20, "xx")
    Error <rlang_error>
      `palette` does not match any given palette

