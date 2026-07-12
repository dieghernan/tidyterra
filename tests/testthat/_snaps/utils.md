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

# pal_gradient_scale validates and creates gradient scales

    Code
      pal_gradient_scale(ggplot2::binned_scale, "fill", pal, n = 2, alpha = -0.1,
      direction = 1, na.value = "transparent", guide = "coloursteps")
    Condition
      Error:
      ! `alpha` must be between 0 and 1.

