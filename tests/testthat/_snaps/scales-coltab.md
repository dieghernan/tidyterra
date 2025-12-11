# Return NULL

    Code
      res <- get_coltab_pal(df)
    Message
      i `tidyterra::get_coltab_pal()` only works with <SpatRaster> objects, not <data.frame>. Returning NULL

---

    Code
      res <- get_coltab_pal(r)
    Message
      i `x` does not have a color table. Returning NULL

# Give informative messages

    Code
      res <- get_coltab_pal(df)
    Message
      i `tidyterra::get_coltab_pal()` only works with <SpatRaster> objects, not <data.frame>. Returning NULL

---

    Code
      res <- get_coltab_pal(r)
    Message
      i `x` does not have a color table. Returning NULL

# Discrete scale color

    Code
      pnull <- p + scale_color_coltab(data = terra::rast())
    Message
      i `x` does not have a color table. Returning NULL

---

    Code
      p + scale_color_coltab(data = r, alpha = -1)
    Condition
      Error in `scale_color_coltab()`:
      ! `alpha` -1 not in [0,1]

# Discrete scale fill

    Code
      pnull <- p + scale_fill_coltab(data = terra::rast())
    Message
      i `x` does not have a color table. Returning NULL

---

    Code
      p + scale_fill_coltab(data = r, alpha = -1)
    Condition
      Error in `scale_fill_coltab()`:
      ! `alpha` -1 not in [0,1]

