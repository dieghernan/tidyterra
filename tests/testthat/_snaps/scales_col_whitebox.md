# Discrete scale

    Code
      p + scale_colour_whitebox_d(alpha = -1)
    Condition
      Error in `scale_colour_whitebox_d()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_whitebox_d(direction = 0.5)
    Condition
      Error in `scale_colour_whitebox_d()`:
      ! `direction` must be 1 or -1

# Continous scale

    Code
      p + scale_colour_whitebox_c(alpha = -1)
    Condition
      Error in `scale_colour_whitebox_c()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_whitebox_c(direction = 0.5)
    Condition
      Error in `scale_colour_whitebox_c()`:
      ! `direction` must be 1 or -1

# Breaking scale

    Code
      p_init + scale_colour_whitebox_b(alpha = -1)
    Condition
      Error in `scale_colour_whitebox_b()`:
      ! `alpha` -1 not in [0,1]

---

    Code
      p + scale_colour_whitebox_b(direction = 0.5)
    Condition
      Error in `scale_colour_whitebox_b()`:
      ! `direction` must be 1 or -1

