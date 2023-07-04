# Discrete scale

    Code
      p + scale_fill_terrain_d(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_terrain_d(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Continous scale

    Code
      p + scale_fill_terrain_c(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_terrain_c(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

# Breaking scale

    Code
      p_init + scale_fill_terrain_b(alpha = -1)
    Error <rlang_error>
      `alpha` -1 not in [0,1]

---

    Code
      p + scale_fill_terrain_b(direction = 0.5)
    Error <rlang_error>
      `direction` must be 1 or -1

