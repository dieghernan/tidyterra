#' Gradient scales for representing hypsometry and bathymetry
#'
#' @description
#'
#' Implementation of a selection of gradient palettes available in
#' [cpt-city](http://soliton.vm.bytemark.co.uk/pub/cpt-city/).
#'
#' The following scales and palettes are provided:
#'
#' - `scale_*_hypso_d()`: For discrete values.
#' - `scale_*_hypso_c()`: For continuous values.
#' - `scale_*_hypso_b()`: For binning continuous values.
#' - `hypso.colors()`: A gradient color palette. See also
#'   [grDevices::terrain.colors()] for details.
#'
#' An additional set of scales is provided. These scales can act as
#' [hypsometric (or bathymetric)
#' tints](https://en.wikipedia.org/wiki/Hypsometric_tints).
#'
#' - `scale_*_hypso_tint_d()`: For discrete values.
#' - `scale_*_hypso_tint_c()`: For continuous values.
#' - `scale_*_hypso_tint_b()`: For binning continuous values.
#' - `hypso.colors2()`: A gradient color palette. See also
#'   [grDevices::terrain.colors()] for details.
#'
#' See **Details**.
#'
#'
#' Additional parameters `...` would be passed on to:
#' - Discrete values: [ggplot2::discrete_scale()]
#' - Continuous values: [ggplot2::continuous_scale()]
#' - Binned continuous values: [ggplot2::binned_scale()].
#'
#' Note that \pkg{tidyterra} just documents a selection of these additional
#' parameters, check the previous links to see the full range of parameters
#' accepted by these scales.
#'
#' @export
#'
#' @name scale_hypso
#'
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
#' @inheritParams scale_cross_blended
#' @param palette A valid palette name. The name is matched to the list of
#'   available palettes, ignoring upper vs. lower case. See
#'   [hypsometric_tints_db] for more info. Values available are:
#'
#' ```{r, echo=FALSE, results="asis", message = FALSE, warning = FALSE}
#'
#' suppressPackageStartupMessages(library(dplyr))
#' hypsometric_tints_db %>%
#'   pull(pal) %>%
#'   unique() %>%
#'   paste0('`"', ., '"`', collapse = ", ") %>%
#'   paste0(".") %>%
#'   cat()
#'
#'
#' ```
#' @seealso [hypsometric_tints_db], [terra::plot()], [terra::minmax()],
#' [ggplot2::scale_fill_viridis_c()]
#'
#' See also \pkg{ggplot2} docs on additional `...` parameters:
#'
#' - `scale_*_terrain_d()`: For discrete values.
#' - `scale_*_terrain_c()`: For continuous values.
#' - `scale_*_terrain_b()`: For binning continuous values.
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill/colour` aesthetics.
#'
#' @family gradients
#'
#' @source
#'
#' cpt-city: <http://soliton.vm.bytemark.co.uk/pub/cpt-city/>.
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' # Palette
#' plot(volcano2_rast, col = hypso.colors(100, palette = "wiki-2.0_hypso"))
#'
#' # Palette with uneven colors
#' plot(volcano2_rast, col = hypso.colors2(100, palette = "wiki-2.0_hypso"))
#'
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_c(palette = "colombia_hypso")
#'
#' # Use hypsometric  tint version...
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_tint_c(palette = "colombia_hypso")
#'
#' # ...but not suitable for the range of the raster: adjust
#' my_lims <- minmax(volcano2_rast) %>% as.integer() + c(-2, 2)
#'
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_tint_c(
#'     palette = "colombia_hypso",
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
#'   scale_fill_hypso_tint_c(
#'     palette = "etopo1",
#'     labels = scales::label_number(),
#'     breaks = c(-10000, 0, 5000, 8000),
#'     guide = guide_colorbar(
#'       direction = "horizontal",
#'       title.position = "top",
#'       barwidth = 25
#'     )
#'   ) +
#'   labs(fill = "elevation (m)") +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#'
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_b(breaks = seq(70, 200, 25), palette = "wiki-2.0_hypso")
#'
#' # With limits and breaks
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_tint_b(
#'     breaks = seq(75, 200, 25),
#'     palette = "wiki-2.0_hypso",
#'     limits = my_lims
#'   )
#'
#' # With discrete values
#' factor <- volcano2_rast %>% mutate(cats = cut(elevation,
#'   breaks = c(100, 120, 130, 150, 170, 200),
#'   labels = c(
#'     "Very Low", "Low", "Average", "High",
#'     "Very High"
#'   )
#' ))
#'
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_hypso_d(na.value = "gray10", palette = "dem_poster")
#'
#'
#' # Tint version
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_hypso_tint_d(na.value = "gray10", palette = "dem_poster")
#' }
scale_fill_hypso_d <- function(palette = "etopo1_hypso", ...,
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
    scale_name = "hypso_fill_d",
    palette = hypso_pal(
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
#' @rdname scale_hypso
scale_colour_hypso_d <- function(palette = "etopo1_hypso", ...,
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
    scale_name = "hypso_colour_d",
    palette = hypso_pal(
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
#' @rdname scale_hypso
scale_fill_hypso_c <- function(palette = "etopo1_hypso", ...,
                               alpha = 1, direction = 1,
                               na.value = "transparent", guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::hypsometric_tints_db,
    palette = palette
  ))

  ggplot2::continuous_scale(
    aesthetics = "fill",
    scale_name = "hypso_fill_c",
    scales::gradient_n_pal(hypso_pal(
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
#' @rdname scale_hypso
scale_colour_hypso_c <- function(palette = "etopo1_hypso", ...,
                                 alpha = 1, direction = 1,
                                 na.value = "transparent",
                                 guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::hypsometric_tints_db,
    palette = palette
  ))

  ggplot2::continuous_scale(
    aesthetics = "colour",
    scale_name = "hypso_colour_c",
    scales::gradient_n_pal(hypso_pal(
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
#' @rdname scale_hypso
scale_fill_hypso_b <- function(palette = "etopo1_hypso", ...,
                               alpha = 1, direction = 1,
                               na.value = "transparent",
                               guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::hypsometric_tints_db,
    palette = palette
  ))
  ggplot2::binned_scale(
    aesthetics = "fill",
    scale_name = "hypso_fill_b",
    scales::gradient_n_pal(hypso_pal(
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
#' @rdname scale_hypso
scale_colour_hypso_b <- function(palette = "etopo1_hypso", ...,
                                 alpha = 1, direction = 1,
                                 na.value = "transparent",
                                 guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::hypsometric_tints_db,
    palette = palette
  ))
  ggplot2::binned_scale(
    aesthetics = "colour",
    scale_name = "hypso_colour_b",
    scales::gradient_n_pal(hypso_pal(
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
#' @rdname scale_hypso
#'
#' @inheritParams wiki.colors
#' @examples
#' # Display all the cpl_city palettes
#'
#' pals <- unique(hypsometric_tints_db$pal)
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
#'     col = hypso.colors(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
hypso.colors <- function(n, palette = "etopo1_hypso",
                         alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(tidyterra::hypsometric_tints_db, palette = palette)
    colors <- as.character(paltab$hex)
    endcols <- tidyterra_ramp(colors, n, alpha, rev)
    return(endcols)
  } else {
    character()
  }
}

#' @export
#' @rdname scale_hypso
#' @details
#'
#' On `scale_*_hypso_tint_*` palettes, the position of the gradients and
#' the limits of the palette are redefined. Instead of treating the color
#' palette as a continuous gradient, they are rescaled to act as a hypsometric
#' tint. A rough description of these tints are:
#' - Blue colors: Negative values.
#' - Green colors: 0 to 1.000 values.
#' - Browns: 1000 to 4.000 values.
#' - Whites: Values higher than 4.000.
#'
#' The following orientation would vary depending on the palette definition
#' (see [hypsometric_tints_db] for an example on how this could be achieved).
#'
#' Note that the setup of the palette may not be always suitable for your
#' specific data. For example, raster of small parts of the globe (and with a
#' limited range of elevations) may not be well represented. As an example, a
#' raster with a range of values on `[100, 200]` would appear almost as an
#' uniform color.
#'
#' This could be adjusted using the `limits`/`values` provided by **ggplot2**.
#'
#' `hypso.colors2()` provides a gradient color palette where the distance
#' between colors is different depending of the type of color. In contrast,
#' `hypso.colors()` provides an uniform gradient across colors.
#' See **Examples**.

scale_fill_hypso_tint_d <- function(palette = "etopo1_hypso", ...,
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
    scale_name = "hypso_tint_fill_d",
    palette = hypso_pal2(
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
#' @rdname scale_hypso
scale_colour_hypso_tint_d <- function(palette = "etopo1_hypso", ...,
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
    scale_name = "hypso_tint_colour_d",
    palette = hypso_pal2(
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
#' @rdname scale_hypso
scale_fill_hypso_tint_c <- function(palette = "etopo1_hypso", ...,
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
  coltab <- tidyterra::hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::hypsometric_tints_db}"
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
#' @rdname scale_hypso
scale_colour_hypso_tint_c <- function(palette = "etopo1_hypso", ...,
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
  coltab <- tidyterra::hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::hypsometric_tints_db}"
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
#' @rdname scale_hypso
scale_fill_hypso_tint_b <- function(palette = "etopo1_hypso", ...,
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
  coltab <- tidyterra::hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::hypsometric_tints_db}"
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
#' @rdname scale_hypso
scale_colour_hypso_tint_b <- function(palette = "etopo1_hypso", ...,
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
  coltab <- tidyterra::hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::hypsometric_tints_db}"
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
#' @rdname scale_hypso
#' @examples
#' # Display all the cpl_city palettes on version 2
#'
#' pals <- unique(hypsometric_tints_db$pal)
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
#'     col = hypso.colors2(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
hypso.colors2 <- function(n, palette = "etopo1_hypso",
                          alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(tidyterra::hypsometric_tints_db, palette = palette)
    colors <- as.character(paltab$hex)
    limits <- sort(as.integer(paltab$limit))
    endcols <- tidyterra_ramp2(colors, n, alpha, rev, limits)
    return(endcols)
  } else {
    character()
  }
}

# Helpers

tidyterra_ramp2 <- function(colors, n, alpha = 1, rev = FALSE, limits) {
  if (rev) colors <- rev(colors)
  # Rescale limits
  limits <- scales::rescale(limits)
  fn_cols <- scales::gradient_n_pal(colors, values = limits)
  endcols <- fn_cols(seq(0, 1, length.out = n))
  if (alpha != 1) endcols <- ggplot2::alpha(endcols, alpha)

  return(endcols)
}


hypso_pal <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- hypso.colors(n,
      rev = direction != 1, alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}


hypso_pal2 <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- hypso.colors2(n,
      rev = direction != 1, alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}

#' @export
#' @rdname scale_hypso
#' @usage NULL
scale_color_hypso_d <- scale_colour_hypso_d

#' @export
#' @rdname scale_hypso
#' @usage NULL
scale_color_hypso_tint_d <- scale_colour_hypso_tint_d


#' @export
#' @rdname scale_hypso
#' @usage NULL
scale_color_hypso_c <- scale_colour_hypso_c

#' @export
#' @rdname scale_hypso
#' @usage NULL
scale_color_hypso_tint_c <- scale_colour_hypso_tint_c


#' @export
#' @rdname scale_hypso
#' @usage NULL
scale_color_hypso_b <- scale_colour_hypso_b

#' @export
#' @rdname scale_hypso
#' @usage NULL
scale_color_hypso_tint_b <- scale_colour_hypso_tint_b
