# check_alpha validates alpha range

    Code
      check_alpha(-0.1)
    Condition
      Error:
      ! `alpha` must be between 0 and 1.

---

    Code
      check_alpha(1.1)
    Condition
      Error:
      ! `alpha` must be between 0 and 1.

# check_alpha_direction validates direction values

    Code
      check_alpha_direction(-0.1, 1)
    Condition
      Error:
      ! `alpha` must be between 0 and 1.

---

    Code
      check_alpha_direction(1, 0)
    Condition
      Error:
      ! `direction` must be either 1 or -1.

---

    Code
      check_alpha_direction(1, 2)
    Condition
      Error:
      ! `direction` must be either 1 or -1.

# check_number_whole_vector validates vectors

    Code
      check_number_whole_vector(c(1, 2), length = 3)
    Condition
      Error:
      ! `c(1, 2)` must be a numeric vector of 3 whole numbers, not <numeric>.

---

    Code
      check_number_whole_vector(1.5)
    Condition
      Error:
      ! `1.5` must be a numeric vector of whole numbers, not <numeric>.

# check_maxcell accepts Inf

    Code
      check_maxcell(0)
    Condition
      Error:
      ! `maxcell` must be a whole number larger than or equal to 1.

---

    Code
      check_maxcell(-Inf)
    Condition
      Error:
      ! `maxcell` must be a whole number larger than or equal to 1.

# check_color_args validates color helper inputs

    Code
      check_color_args(1.5, 1, FALSE, n_arg = "n")
    Condition
      Error:
      ! `n` must be a whole number, not the number 1.5.

---

    Code
      check_color_args(1, 1.1, FALSE)
    Condition
      Error:
      ! `alpha` must be between 0 and 1.

---

    Code
      check_color_args(1, 1, "FALSE", rev_arg = "rev")
    Condition
      Error:
      ! `rev` must be `TRUE` or `FALSE`, not the string "FALSE".

# check_spatraster validates SpatRaster inputs

    Code
      check_spatraster(data.frame(x = 1), "test_fn")
    Condition
      Error:
      ! `tidyterra::test_fn()` only works with <SpatRaster> objects, not <data.frame>. See `?terra::rast()`.

# warn_overlapping_layers warns on overlapping layers

    Code
      warn_overlapping_layers(data, "test_fn")
    Message
      ! `tidyterra::test_fn()`: Plotting 2 overlapping layers: "a" and "b". Either:
      * Use `facet_wrap(~lyr)` to facet layers.
      * Use `aes(fill = <name_of_layer>)` to display a single layer.

# pal_discrete_scale validates and creates a discrete scale

    Code
      pal_discrete_scale("fill", function(n) grDevices::gray.colors(n), alpha = 1,
      direction = 0, na.translate = FALSE, drop = TRUE)
    Condition
      Error:
      ! `direction` must be either 1 or -1.

---

    Code
      pal_discrete_scale("fill", function(n) grDevices::gray.colors(n), alpha = 1,
      direction = 1, na.translate = "yes", drop = TRUE)
    Condition
      Error:
      ! `na.translate` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      pal_discrete_scale("fill", function(n) grDevices::gray.colors(n), alpha = 1,
      direction = 1, na.translate = TRUE, drop = "yes")
    Condition
      Error:
      ! `drop` must be `TRUE` or `FALSE`, not the string "yes".

# pal_gradient_scale validates and creates gradient scales

    Code
      pal_gradient_scale(ggplot2::binned_scale, "fill", pal, n = 2, alpha = -0.1,
      direction = 1, na.value = "transparent", guide = "coloursteps")
    Condition
      Error:
      ! `alpha` must be between 0 and 1.

