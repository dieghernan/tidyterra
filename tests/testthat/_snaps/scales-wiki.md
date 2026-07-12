# Discrete scale

    Code
      p + scale_fill_wiki_d(alpha = -1)
    Condition
      Error in `scale_fill_wiki_d()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_wiki_d(direction = 0.5)
    Condition
      Error in `scale_fill_wiki_d()`:
      ! `direction` must be either 1 or -1.

# Continous scale

    Code
      p + scale_fill_wiki_c(alpha = -1)
    Condition
      Error in `scale_fill_wiki_c()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_wiki_c(direction = 0.5)
    Condition
      Error in `scale_fill_wiki_c()`:
      ! `direction` must be either 1 or -1.

# Breaking scale

    Code
      p_init + scale_fill_wiki_b(alpha = -1)
    Condition
      Error in `scale_fill_wiki_b()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_fill_wiki_b(direction = 0.5)
    Condition
      Error in `scale_fill_wiki_b()`:
      ! `direction` must be either 1 or -1.

# Palette

    Code
      wiki.colors(20, "xx")
    Condition
      Error in `wiki.colors()`:
      ! `alpha` must be a number, not the string "xx".

---

    Code
      wiki.colors(1.5)
    Condition
      Error in `wiki.colors()`:
      ! `n` must be a whole number, not the number 1.5.

---

    Code
      wiki.colors(1, rev = "FALSE")
    Condition
      Error in `wiki.colors()`:
      ! `rev` must be `TRUE` or `FALSE`, not the string "FALSE".

# Discrete scale col

    Code
      p + scale_colour_wiki_d(alpha = -1)
    Condition
      Error in `scale_colour_wiki_d()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_wiki_d(direction = 0.5)
    Condition
      Error in `scale_colour_wiki_d()`:
      ! `direction` must be either 1 or -1.

# Continous scale col

    Code
      p + scale_colour_wiki_c(alpha = -1)
    Condition
      Error in `scale_colour_wiki_c()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_wiki_c(direction = 0.5)
    Condition
      Error in `scale_colour_wiki_c()`:
      ! `direction` must be either 1 or -1.

# Breaking scale col

    Code
      p_init + scale_colour_wiki_b(alpha = -1)
    Condition
      Error in `scale_colour_wiki_b()`:
      ! `alpha` must be between 0 and 1.

---

    Code
      p + scale_colour_wiki_b(direction = 0.5)
    Condition
      Error in `scale_colour_wiki_b()`:
      ! `direction` must be either 1 or -1.

