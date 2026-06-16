#' Cross blended hypsometric tints scales
#'
#' @description
#'
#' Implementation of the cross blended hypsometric gradients presented on
#' \doi{10.14714/CP69.20}. The following fill scales and palettes are provided:
#'
#' - `scale_*_cross_blended_d()`: For discrete values.
#' - `scale_*_cross_blended_c()`: For continuous values.
#' - `scale_*_cross_blended_b()`: For binning continuous values.
#' - `cross_blended.colors()`: A gradient color palette. See also
#'   [grDevices::terrain.colors()] for details.
#'
#' An additional set of scales is provided. These scales can act as
#' [hypsometric (or bathymetric)
#' tints](https://en.wikipedia.org/wiki/Hypsometric_tints).
#'
#' - `scale_*_cross_blended_tint_d()`: For discrete values.
#' - `scale_*_cross_blended_tint_c()`: For continuous values.
#' - `scale_*_cross_blended_tint_b()`: For binning continuous values.
#' - `cross_blended.colors2()`: A gradient color palette. See also
#'   [grDevices::terrain.colors()] for details.
#'
#' See **Details**.
#'
#' Additional arguments `...` are passed to:
#' - Discrete values: [ggplot2::discrete_scale()].
#' - Continuous values: [ggplot2::continuous_scale()].
#' - Binned continuous values: [ggplot2::binned_scale()].
#'
#' \CRANpkg{tidyterra} documents only a selection of these additional
#' arguments, so check the \CRANpkg{ggplot2} functions listed above to see the
#' full range of arguments accepted by these scales.
#'
#' @source
#'
#' - Patterson, T., & Jenny, B. (2011). The Development and Rationale of
#'   Cross-blended Hypsometric Tints. *Cartographic Perspectives,* (69), 31-46.
#'   \doi{10.14714/CP69.20}.
#'
#' - Patterson, T. (2004). *Using Cross-blended Hypsometric Tints for
#'   Generalized Environmental Mapping.* Online, Accessed June 10, 2022.
#'
#' @export
#' @encoding UTF-8
#'
#' @name scale_cross_blended
#'
#' @seealso [cross_blended_hypsometric_tints_db], [terra::plot()],
#' [terra::minmax()], [ggplot2::scale_fill_viridis_c()].
#'
#' See also \CRANpkg{ggplot2} docs on additional `...` arguments.
#'
#' @family gradients
#'
#' @inheritParams ggplot2::scale_fill_viridis_b
#' @inheritParams ggplot2::continuous_scale
#'
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
#' @param na.translate Logical. If `TRUE`, remove `NA` values from the legend.
#'   The default is `TRUE`.
#' @param na.value Missing values will be replaced with this value. By default,
#'   \CRANpkg{tidyterra} uses `na.value = "transparent"` so cells with `NA` are
#'   not filled. See also
#'   [#120](https://github.com/dieghernan/tidyterra/issues/120).
#'
#' @param drop Logical. If `TRUE`, omit unused factor levels from the scale.
#'   The default (`TRUE`) removes unused factors.
#' @param palette A valid palette name. The name is matched to the list of
#'   available palettes, ignoring upper vs. lower case. See
#'   [cross_blended_hypsometric_tints_db] for more information. The available
#'   values are listed below.
#'
#' ```{r, echo=FALSE, results="asis", message = FALSE, warning = FALSE}
#'
#' suppressPackageStartupMessages(library(dplyr))
#' cross_blended_hypsometric_tints_db |>
#'   pull(pal) |>
#'   unique() %>%
#'   paste0('`"', ., '"`', collapse = ", ") |>
#'   paste0(".") |>
#'   cat()
#'
#' ```
#' @returns
#' The corresponding \CRANpkg{ggplot2} layer with the values applied to the
#' `fill/colour` aesthetics.
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' # Palette
#' plot(volcano2_rast, col = cross_blended.colors(100, palette = "arid"))
#'
#' # Palette with uneven colors
#' plot(volcano2_rast, col = cross_blended.colors2(100, palette = "arid"))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_cross_blended_c(palette = "cold_humid")
#'
#' # Full map with true tints
#'
#' f_asia <- system.file("extdata/asia.tif", package = "tidyterra")
#' asia <- rast(f_asia)
#'
#' ggplot() +
#'   geom_spatraster(data = asia) +
#'   scale_fill_cross_blended_tint_c(
#'     palette = "warm_humid",
#'     labels = scales::label_number(),
#'     breaks = c(-10000, 0, 5000, 8000),
#'     guide = guide_colorbar(reverse = TRUE)
#'   ) +
#'   labs(fill = "elevation (m)") +
#'   theme(
#'     legend.position = "bottom",
#'     legend.title.position = "top",
#'     legend.key.width = rel(3),
#'     legend.ticks = element_line(colour = "black", linewidth = 0.3),
#'     legend.direction = "horizontal"
#'   )
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_cross_blended_b(breaks = seq(70, 200, 25), palette = "arid")
#'
#' # With breaks
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_cross_blended_b(
#'     breaks = seq(75, 200, 25),
#'     palette = "arid"
#'   )
#'
#' # With discrete values
#' factor <- volcano2_rast |>
#'   mutate(cats = cut(elevation,
#'     breaks = c(100, 120, 130, 150, 170, 200),
#'     labels = c(
#'       "Very Low", "Low", "Average", "High",
#'       "Very High"
#'     )
#'   ))
#'
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_cross_blended_d(na.value = "gray10", palette = "cold_humid")
#'
#' # Tint version
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_cross_blended_tint_d(
#'     na.value = "gray10",
#'     palette = "cold_humid"
#'   )
#' }
scale_fill_cross_blended_d <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "fill",
    cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}
#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_colour_cross_blended_d <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "colour",
    cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_fill_cross_blended_c <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  pal_gradient_scale(
    ggplot2::continuous_scale,
    "fill",
    cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() {
      nrow(extract_pal(
        tidyterra::cross_blended_hypsometric_tints_db,
        palette = palette
      ))
    },
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_colour_cross_blended_c <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  pal_gradient_scale(
    ggplot2::continuous_scale,
    "colour",
    cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() {
      nrow(extract_pal(
        tidyterra::cross_blended_hypsometric_tints_db,
        palette = palette
      ))
    },
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}
#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_fill_cross_blended_b <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  pal_gradient_scale(
    ggplot2::binned_scale,
    "fill",
    cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() {
      nrow(extract_pal(
        tidyterra::cross_blended_hypsometric_tints_db,
        palette = palette
      ))
    },
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}
#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_colour_cross_blended_b <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  pal_gradient_scale(
    ggplot2::binned_scale,
    "colour",
    cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() {
      nrow(extract_pal(
        tidyterra::cross_blended_hypsometric_tints_db,
        palette = palette
      ))
    },
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#'
#' @inheritParams wiki.colors
#' @examples
#' # Display all the cross-blended palettes
#'
#' pals <- unique(cross_blended_hypsometric_tints_db$pal)
#'
#' # Helper function for plotting
#'
#' ncols <- 128
#' rowcol <- grDevices::n2mfrow(length(pals))
#'
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = rowcol, mar = rep(1, 4))
#'
#' for (i in pals) {
#'   image(
#'     x = seq(1, ncols), y = 1, z = as.matrix(seq(1, ncols)),
#'     col = cross_blended.colors(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
cross_blended.colors <- function(
  n,
  palette = "cold_humid",
  alpha = 1,
  rev = FALSE
) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(
      tidyterra::cross_blended_hypsometric_tints_db,
      palette = palette
    )
    colors <- as.character(paltab$hex)
    endcols <- tidyterra_ramp(colors, n, alpha, rev)
    endcols
  } else {
    character()
  }
}

#' @details
#'
#' On `scale_*_cross_blended_tint_*` palettes, the position of the gradients
#' and the limits of the palette are redefined. Instead of treating the color
#' palette as a continuous gradient, they are rescaled to act as a hypsometric
#' tint. A rough description of these tints are:
#' - Blue colors: Negative values.
#' - Green colors: 0 to 1.000 values.
#' - Browns: 1000 to 4.000 values.
#' - Whites: Values higher than 4.000.
#'
#' The following orientation varies depending on the palette definition (see
#' [cross_blended_hypsometric_tints_db] for an example of how this can be
#' achieved).
#'
#' The palette setup may not always be suitable for your specific data. For
#' example, a `SpatRaster` of small parts of the globe (and
#' with a limited range of elevations) may not be well represented. As an
#' example, a `SpatRaster` with a range of values on `[100, 200]` appears
#' almost as a uniform color. This can be adjusted using the `limits`/`values`
#' arguments.
#'
#' When passing the `limits` argument to `scale_*_cross_blended_tint_*`, the
#' colors are restricted to those specified by this argument, keeping the
#' distribution of the tint. You can combine this with `oob`, for example
#' `oob = scales::oob_squish`, to avoid blank pixels in the plot.
#'
#' `cross_blended.colors2()` provides a gradient color palette where the
#' distance between colors is different depending of the type of color.
#' In contrast, `cross_blended.colors()` provides a uniform gradient across
#' colors. See **Examples**.
#'
#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_fill_cross_blended_tint_d <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "fill",
    cross_blended_pal2(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_colour_cross_blended_tint_d <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "colour",
    cross_blended_pal2(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}
#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_fill_cross_blended_tint_c <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  na.value = "transparent",
  guide = "colourbar"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- tint_scale_params(
    coltab = tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    help = "tidyterra::cross_blended_hypsometric_tints_db"
  )

  ggplot2::scale_fill_gradientn(
    ...,
    colors = scale_params$colors,
    values = scale_params$values,
    limits = scale_params$limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_colour_cross_blended_tint_c <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  na.value = "transparent",
  guide = "colourbar"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- tint_scale_params(
    coltab = tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    help = "tidyterra::cross_blended_hypsometric_tints_db"
  )

  ggplot2::scale_colour_gradientn(
    ...,
    colors = scale_params$colors,
    values = scale_params$values,
    limits = scale_params$limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_fill_cross_blended_tint_b <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  na.value = "transparent",
  guide = "coloursteps"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- tint_scale_params(
    coltab = tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    help = "tidyterra::cross_blended_hypsometric_tints_db"
  )

  ggplot2::scale_fill_stepsn(
    ...,
    colors = scale_params$colors,
    values = scale_params$values,
    limits = scale_params$limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
scale_colour_cross_blended_tint_b <- function(
  palette = "cold_humid",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  na.value = "transparent",
  guide = "coloursteps"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- tint_scale_params(
    coltab = tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    help = "tidyterra::cross_blended_hypsometric_tints_db"
  )

  ggplot2::scale_colour_stepsn(
    ...,
    colors = scale_params$colors,
    values = scale_params$values,
    limits = scale_params$limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#' @examples
#' # Display all the cross-blended palettes on version 2
#'
#' pals <- unique(cross_blended_hypsometric_tints_db$pal)
#'
#' # Helper function for plotting
#'
#' ncols <- 128
#' rowcol <- grDevices::n2mfrow(length(pals))
#'
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = rowcol, mar = rep(1, 4))
#'
#' for (i in pals) {
#'   image(
#'     x = seq(1, ncols), y = 1, z = as.matrix(seq(1, ncols)),
#'     col = cross_blended.colors2(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
cross_blended.colors2 <- function(
  n,
  palette = "cold_humid",
  alpha = 1,
  rev = FALSE
) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(
      tidyterra::cross_blended_hypsometric_tints_db,
      palette = palette
    )
    colors <- as.character(paltab$hex)
    limits <- sort(as.integer(paltab$limit))
    endcols <- tidyterra_ramp2(colors, n, alpha, rev, limits)
    endcols
  } else {
    character()
  }
}

# Helpers
cross_blended_pal <- function(alpha = 1, direction = 1, palette) {
  function(n) {
    pal <- cross_blended.colors(
      n,
      rev = direction != 1,
      alpha = alpha,
      palette = palette
    )

    pal
  }
}

cross_blended_pal2 <- function(alpha = 1, direction = 1, palette) {
  function(n) {
    pal <- cross_blended.colors2(
      n,
      rev = direction != 1,
      alpha = alpha,
      palette = palette
    )

    pal
  }
}

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_d <- scale_colour_cross_blended_d

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_tint_d <- scale_colour_cross_blended_tint_d

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_c <- scale_colour_cross_blended_c

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_tint_c <- scale_colour_cross_blended_tint_c

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_b <- scale_colour_cross_blended_b

#' @export
#' @encoding UTF-8
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_tint_b <- scale_colour_cross_blended_tint_b
