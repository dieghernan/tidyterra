#' GRASS scales
#'
#' @description
#'
#' Implementation of [GRASS color
#' tables](https://grass.osgeo.org/grass83/manuals/r.colors.html). The
#' following fill scales and palettes are provided:
#'
#' - `scale_*_grass_d()`: For discrete values.
#' - `scale_*_grass_c()`: For continuous values.
#' - `scale_*_grass_b()`: For binning continuous values.
#' - `grass.colors()`: Gradient color palette. See also
#'   [grDevices::terrain.colors()] for details.
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
#' These palettes are an implementation of [terra::map.pal()], that is the
#' default color palettes provided by [terra::plot()] (\CRANpkg{terra}
#' `> 1.7.78`).
#'
#' @export
#'
#' @name scale_grass
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
#'   [grass_db] for more info.
#'
#' @param use_grass_range Logical. Should the scale use the suggested range
#'   when plotting? See **Details**.
#' @seealso [grass_db], [terra::plot()],
#' [terra::minmax()], [ggplot2::scale_fill_viridis_c()].
#'
#' See also \CRANpkg{ggplot2} docs on additional `...` parameters:
#'
#' @return
#' The corresponding \CRANpkg{ggplot2} layer with the values applied to the
#' `fill/colour` `aes()`.
#'
#' @family gradients
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::map.pal()]
#'
#' @source
#'
#' Derived from <https://github.com/OSGeo/grass/tree/main/lib/gis/colors>. See
#' also [r.color - GRASS GIS
#' Manual](https://grass.osgeo.org/grass83/manuals/r.colors.html).
#'
#' @references
#' GRASS Development Team (2024). *Geographic Resources Analysis Support System
#' (GRASS) Software, Version 8.3.2*. Open Source Geospatial Foundation, USA.
#' <https://grass.osgeo.org>.
#'
#' @details
#' Some palettes are mapped by default to a specific range of values (see
#' [grass_db]). However, it is possible to modify this behaviour with the
#' `use_grass_range` argument, When `FALSE` the color scales would be mapped
#' to the range of values of the `color/fill` aesthethics, See **Examples**.
#'
#' When passing `limits` parameter the colors would be restricted of those
#' specified by this parameter, keeping the distribution of the palette. You can
#' combine this with `oob` (i.e. `oob = scales::oob_squish`) to avoid blank
#' pixels in the plot.
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' # Palette
#' plot(volcano2_rast, col = grass.colors(100, palette = "haxby"))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_grass_c(palette = "terrain")
#'
#' # Use with no default limits
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_grass_c(palette = "terrain", use_grass_range = FALSE)
#'
#' # Full map with true tints
#'
#' f_asia <- system.file("extdata/asia.tif", package = "tidyterra")
#' asia <- rast(f_asia)
#'
#' ggplot() +
#'   geom_spatraster(data = asia) +
#'   scale_fill_grass_c(
#'     palette = "srtm_plus",
#'     labels = scales::label_number(),
#'     breaks = c(-10000, 0, 5000, 8000),
#'     guide = guide_colorbar(reverse = FALSE)
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
#'   scale_fill_grass_b(breaks = seq(70, 200, 25), palette = "sepia")
#'
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
#'   scale_fill_grass_d(palette = "soilmoisture")
#' }
scale_fill_grass_d <- function(palette = "viridis", ...,
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
    palette = grass_pal(
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
#' @rdname scale_grass
scale_colour_grass_d <- function(palette = "viridis", ...,
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
    palette = grass_pal(
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
#' @rdname scale_grass
#' @usage NULL
scale_color_grass_d <- scale_colour_grass_d

#' @export
#' @rdname scale_grass
scale_fill_grass_c <- function(palette = "viridis", ...,
                               alpha = 1, direction = 1,
                               values = NULL,
                               limits = NULL,
                               use_grass_range = TRUE,
                               na.value = "transparent",
                               guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::grass_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::grass_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  # Check if use grass range
  if (any(!use_grass_range, anyNA(hypsocol$limit))) {
    limits <- limits
    res <- values
    if (!is.null(values)) res <- scales::rescale(res)
  } else {
    if (is.null(values)) values <- hypsocol$limit
    # Reescale
    if (is.null(limits)) limits <- range(values)
    res <- scales::rescale(values, from = limits)
  }

  ggplot2::scale_fill_gradientn(...,
    colors = hexcol,
    values = res,
    limits = limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @rdname scale_grass
scale_colour_grass_c <- function(palette = "viridis", ...,
                                 alpha = 1, direction = 1,
                                 values = NULL,
                                 limits = NULL,
                                 use_grass_range = TRUE,
                                 na.value = "transparent",
                                 guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::grass_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::grass_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  # Check if use grass range
  if (any(!use_grass_range, anyNA(hypsocol$limit))) {
    limits <- limits
    res <- values
    if (!is.null(values)) res <- scales::rescale(res)
  } else {
    if (is.null(values)) values <- hypsocol$limit
    # Reescale
    if (is.null(limits)) limits <- range(values)
    res <- scales::rescale(values, from = limits)
  }

  ggplot2::scale_colour_gradientn(...,
    colors = hexcol,
    values = res,
    limits = limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @rdname scale_grass
#' @usage NULL
scale_color_grass_c <- scale_colour_grass_c

#' @export
#' @rdname scale_grass
scale_fill_grass_b <- function(palette = "viridis", ...,
                               alpha = 1, direction = 1,
                               values = NULL,
                               limits = NULL,
                               use_grass_range = TRUE,
                               na.value = "transparent",
                               guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::grass_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::grass_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  # Check if use grass range
  if (any(!use_grass_range, anyNA(hypsocol$limit))) {
    limits <- limits
    res <- values
    if (!is.null(values)) res <- scales::rescale(res)
  } else {
    if (is.null(values)) values <- hypsocol$limit
    # Reescale
    if (is.null(limits)) limits <- range(values)
    res <- scales::rescale(values, from = limits)
  }

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
#' @rdname scale_grass
scale_colour_grass_b <- function(palette = "viridis", ...,
                                 alpha = 1, direction = 1,
                                 values = NULL,
                                 limits = NULL,
                                 use_grass_range = TRUE,
                                 na.value = "transparent",
                                 guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  # Use pal limits
  coltab <- tidyterra::grass_db

  if (!palette %in% coltab$pal) {
    cli::cli_abort(paste(
      "{.arg palette} {.val palette} does not match any given palette.",
      "See {.help tidyterra::grass_db}"
    ))
  }

  hypsocol <- coltab[coltab$pal == palette, ]
  hexcol <- as.character(hypsocol$hex)
  if (direction == -1) hexcol <- rev(hexcol)
  if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

  # Check if use grass range
  if (any(!use_grass_range, anyNA(hypsocol$limit))) {
    limits <- limits
    res <- values
    if (!is.null(values)) res <- scales::rescale(res)
  } else {
    if (is.null(values)) values <- hypsocol$limit
    # Reescale
    if (is.null(limits)) limits <- range(values)
    res <- scales::rescale(values, from = limits)
  }

  ggplot2::scale_color_stepsn(
    ...,
    colors = hexcol,
    values = res,
    limits = limits,
    na.value = na.value,
    guide = guide
  )
}

#' @export
#' @rdname scale_grass
#' @usage NULL
scale_color_grass_b <- scale_colour_grass_b

#' @export
#' @rdname scale_grass
#'
#' @inheritParams wiki.colors
#' @examples
#'
#' # Display all the GRASS palettes
#' data("grass_db")
#'
#' pals_all <- unique(grass_db$pal)
#'
#' # In batches
#' pals <- pals_all[c(1:25)]
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
#'     col = grass.colors(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
#'
#' # Second batch
#' pals <- pals_all[-c(1:25)]
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
#'     col = grass.colors(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
grass.colors <- function(n, palette = "viridis",
                         alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(tidyterra::grass_db, palette = palette)
    colors <- as.character(paltab$hex)
    if (anyNA(paltab$limit)) {
      endcols <- tidyterra_ramp(colors, n, alpha, rev)
    } else {
      endcols <- tidyterra_ramp2(colors, n, alpha, rev,
        limits = as.vector(paltab$limit)
      )
    }

    return(endcols)
  } else {
    character()
  }
}

# Helpers
grass_pal <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- grass.colors(n,
      rev = direction != 1, alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}
