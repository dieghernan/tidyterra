# Discrete scale

    Code
      p + scale_fill_whitebox_d(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_whitebox_d(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Continous scale

    Code
      p + scale_fill_whitebox_c(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_whitebox_c(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Breaking scale

    Code
      p_init + scale_fill_whitebox_b(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_whitebox_b(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Palette

    Code
      whitebox.colors(20, "xx")
    Error <rlang_error>
      `palette` does not match any given palette

