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
#' Additional parameters `...` would be passed on to:
#' - Discrete values: [ggplot2::discrete_scale()].
#' - Continuous values: [ggplot2::continuous_scale()].
#' - Binned continuous values: [ggplot2::binned_scale()].
#'
#' **Note that** \CRANpkg{tidyterra} just documents a selection of these
#' additional parameters, check the \CRANpkg{ggplot2} functions listed above to
#' see the full range of parameters accepted by these scales.
#'
#' @export
#'
#' @name scale_cross_blended
#'
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
#' @param na.translate Should `NA` values be removed from the legend? Default
#'   is `TRUE`.
#' @param na.value Missing values will be replaced with this value. By default,
#'   \CRANpkg{tidyterra} uses `na.value = "transparent"` so cells with `NA` are
#'   not filled. See also
#'   [#120](https://github.com/dieghernan/tidyterra/issues/120).
#'
#' @param drop Should unused factor levels be omitted from the scale? The
#'   default (`TRUE`) removes unused factors.
#' @inheritParams ggplot2::scale_fill_viridis_b
#' @inheritParams ggplot2::continuous_scale
#'
#' @param palette A valid palette name. The name is matched to the list of
#'   available palettes, ignoring upper vs. lower case. See
#'   [cross_blended_hypsometric_tints_db] for more info. Values available are:
#'
#' ```{r, echo=FALSE, results="asis", message = FALSE, warning = FALSE}
#'
#' suppressPackageStartupMessages(library(dplyr))
#' cross_blended_hypsometric_tints_db %>%
#'   pull(pal) %>%
#'   unique() %>%
#'   paste0('`"', ., '"`', collapse = ", ") %>%
#'   paste0(".") %>%
#'   cat()
#'
#'
#' ```
#' @seealso [cross_blended_hypsometric_tints_db], [terra::plot()],
#' [terra::minmax()], [ggplot2::scale_fill_viridis_c()].
#'
#' See also \CRANpkg{ggplot2} docs on additional `...` parameters.
#'
#' @return
#' The corresponding \CRANpkg{ggplot2} layer with the values applied to the
#' `fill/colour` aesthetics.
#'
#' @family gradients
#'
#' @source
#'
#' - Patterson, T., & Jenny, B. (2011). The Development and Rationale of
#'   Cross-blended Hypsometric Tints. *Cartographic Perspectives,* (69), 31 -
#'   46. \doi{10.14714/CP69.20}.
#'
#' - Patterson, T. (2004). *Using Cross-blended Hypsometric Tints for
#'   Generalized Environmental Mapping.* Accessed June 10, 2022.
#'   <https://www.shadedrelief.com/hypso/hypso.html>
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
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_cross_blended_c(palette = "cold_humid")
#'
#' # Use hypsometric  tint version...
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_cross_blended_tint_c(palette = "cold_humid")
#'
#' # ...but not suitable for the range of the raster: adjust
#' my_lims <- minmax(volcano2_rast) %>% as.integer() + c(-2, 2)
#'
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_cross_blended_tint_c(
#'     palette = "cold_humid",
#'     limits = my_lims
#'   )
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
#' # With limits and breaks
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_cross_blended_tint_b(
#'     breaks = seq(75, 200, 25),
#'     palette = "arid",
#'     limits = my_lims
#'   )
#'
#' # With discrete values
#' factor <- volcano2_rast %>%
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
#'
#' # Tint version
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_cross_blended_tint_d(
#'     na.value = "gray10",
#'     palette = "cold_humid"
#'   )
#' }
scale_fill_cross_blended_d <- function(palette = "cold_humid", ...,
                                       alpha = 1, direction = 1,
                                       na.translate = FALSE,
                                       drop = TRUE) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}
#' @export
#' @rdname scale_cross_blended
scale_colour_cross_blended_d <- function(palette = "cold_humid", ...,
                                         alpha = 1, direction = 1,
                                         na.translate = FALSE, drop = TRUE) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @rdname scale_cross_blended
scale_fill_cross_blended_c <- function(palette = "cold_humid", ...,
                                       alpha = 1, direction = 1,
                                       na.value = "transparent",
                                       guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette
  ))

  ggplot2::continuous_scale(
    aesthetics = "fill",
    palette = scales::gradient_n_pal(cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_cross_blended
scale_colour_cross_blended_c <- function(palette = "cold_humid", ...,
                                         alpha = 1, direction = 1,
                                         na.value = "transparent",
                                         guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette
  ))

  ggplot2::continuous_scale(
    aesthetics = "colour",
    palette = scales::gradient_n_pal(cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}
#' @export
#' @rdname scale_cross_blended
scale_fill_cross_blended_b <- function(palette = "cold_humid", ...,
                                       alpha = 1, direction = 1,
                                       na.value = "transparent",
                                       guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette
  ))
  ggplot2::binned_scale(
    aesthetics = "fill",
    palette = scales::gradient_n_pal(cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}
#' @export
#' @rdname scale_cross_blended
scale_colour_cross_blended_b <- function(palette = "cold_humid", ...,
                                         alpha = 1, direction = 1,
                                         na.value = "transparent",
                                         guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::cross_blended_hypsometric_tints_db,
    palette = palette
  ))
  ggplot2::binned_scale(
    aesthetics = "colour",
    palette = scales::gradient_n_pal(cross_blended_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_cross_blended
#'
#' @inheritParams wiki.colors
#' @examples
#' # Display all the cross-blended palettes
#'
#' pals <- unique(cross_blended_hypsometric_tints_db$pal)
#'
#' # Helper fun for plotting
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
cross_blended.colors <- function(n, palette = "cold_humid",
                                 alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(tidyterra::cross_blended_hypsometric_tints_db,
      palette = palette
    )
    colors <- as.character(paltab$hex)
    endcols <- tidyterra_ramp(colors, n, alpha, rev)
    return(endcols)
  } else {
    character()
  }
}

#' @export
#' @rdname scale_cross_blended
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
#' The following orientation would vary depending on the palette definition
#' (see [cross_blended_hypsometric_tints_db] for an example on how this could
#' be achieved).
#'
#' Note that the setup of the palette may not be always suitable for your
#' specific data. For example, a `SpatRaster` of small parts of the globe (and
#' with a limited range of elevations) may not be well represented. As an
#' example, a `SpatRaster` with a range of values on `[100, 200]` would appear
#' almost as an uniform color. This could be adjusted using the
#' `limits`/`values` parameters.
#'
#' `cross_blended.colors2()` provides a gradient color palette where the
#' distance between colors is different depending of the type of color.
#' In contrast, `cross_blended.colors()` provides an uniform gradient across
#' colors. See **Examples**.
#'
scale_fill_cross_blended_tint_d <- function(palette = "cold_humid", ...,
                                            alpha = 1, direction = 1,
                                            na.translate = FALSE,
                                            drop = TRUE) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = cross_blended_pal2(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @rdname scale_cross_blended
scale_colour_cross_blended_tint_d <- function(palette = "cold_humid", ...,
                                              alpha = 1, direction = 1,
                                              na.translate = FALSE,
                                              drop = TRUE) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = cross_blended_pal2(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}
#' @export
#' @rdname scale_cross_blended
scale_fill_cross_blended_tint_c <- function(palette = "cold_humid", ...,
                                            alpha = 1, direction = 1,
                                            values = NULL,
                                            limits = NULL,
                                            na.value = "transparent",
                                            guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::cross_blended_hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::cross_blended_hypsometric_tints_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  if (is.null(values)) values <- hypsocol$limit
  # Reescale
  res <- scales::rescale(values)
  if (is.null(limits)) limits <- range(values)

  ggplot2::scale_fill_gradientn(...,
    colors = hexcol,
    values = res,
    limits = limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @rdname scale_cross_blended
scale_colour_cross_blended_tint_c <- function(palette = "cold_humid", ...,
                                              alpha = 1, direction = 1,
                                              values = NULL,
                                              limits = NULL,
                                              na.value = "transparent",
                                              guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::cross_blended_hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::cross_blended_hypsometric_tints_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  if (is.null(values)) values <- hypsocol$limit
  # Reescale
  res <- scales::rescale(values)
  if (is.null(limits)) limits <- range(values)

  ggplot2::scale_colour_gradientn(...,
    colors = hexcol,
    values = res,
    limits = limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @rdname scale_cross_blended
scale_fill_cross_blended_tint_b <- function(palette = "cold_humid", ...,
                                            alpha = 1, direction = 1,
                                            values = NULL,
                                            limits = NULL,
                                            na.value = "transparent",
                                            guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::cross_blended_hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::cross_blended_hypsometric_tints_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  if (is.null(values)) values <- hypsocol$limit
  # Reescale
  res <- scales::rescale(values)
  if (is.null(limits)) limits <- range(values)

  ggplot2::scale_fill_stepsn(
    ...,
    colors = hexcol,
    values = res,
    limits = limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @rdname scale_cross_blended
scale_colour_cross_blended_tint_b <- function(palette = "cold_humid", ...,
                                              alpha = 1, direction = 1,
                                              values = NULL,
                                              limits = NULL,
                                              na.value = "transparent",
                                              guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::cross_blended_hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::cross_blended_hypsometric_tints_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  if (is.null(values)) values <- hypsocol$limit
  # Reescale
  res <- scales::rescale(values)
  if (is.null(limits)) limits <- range(values)

  ggplot2::scale_colour_stepsn(
    ...,
    colors = hexcol,
    values = res,
    limits = limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @rdname scale_cross_blended
#' @examples
#' # Display all the cross-blended palettes on version 2
#'
#' pals <- unique(cross_blended_hypsometric_tints_db$pal)
#'
#' # Helper fun for plotting
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
cross_blended.colors2 <- function(n, palette = "cold_humid",
                                  alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(tidyterra::cross_blended_hypsometric_tints_db,
      palette = palette
    )
    colors <- as.character(paltab$hex)
    limits <- sort(as.integer(paltab$limit))
    endcols <- tidyterra_ramp2(colors, n, alpha, rev, limits)
    return(endcols)
  } else {
    character()
  }
}

# Helpers
cross_blended_pal <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- cross_blended.colors(n,
      rev = direction != 1, alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}


cross_blended_pal2 <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- cross_blended.colors2(n,
      rev = direction != 1, alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}


#' @export
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_d <- scale_colour_cross_blended_d

#' @export
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_tint_d <- scale_colour_cross_blended_tint_d


#' @export
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_c <- scale_colour_cross_blended_c

#' @export
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_tint_c <- scale_colour_cross_blended_tint_c


#' @export
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_b <- scale_colour_cross_blended_b

#' @export
#' @rdname scale_cross_blended
#' @usage NULL
scale_color_cross_blended_tint_b <- scale_colour_cross_blended_tint_b
