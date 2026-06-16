#' GRASS scales
#'
#' @description
#'
#' Implementation of [GRASS color
#' tables](https://grass.osgeo.org/grass-stable/manuals/r.colors.html). The
#' following fill scales and palettes are provided:
#'
#' - `scale_*_grass_d()`: For discrete values.
#' - `scale_*_grass_c()`: For continuous values.
#' - `scale_*_grass_b()`: For binning continuous values.
#' - `grass.colors()`: Gradient color palette. See also
#'   [grDevices::terrain.colors()] for details.
#'
#' Additional arguments `...` are passed to:
#' - Discrete values: [ggplot2::discrete_scale()].
#' - Continuous values: [ggplot2::continuous_scale()].
#' - Binned continuous values: [ggplot2::binned_scale()].
#'
#' \CRANpkg{tidyterra} documents only a subset of these additional arguments,
#' so see the \CRANpkg{ggplot2} functions listed above for the full range.
#'
#' These palettes implement [terra::map.pal()], the default color palettes used
#' by [terra::plot()] in \CRANpkg{terra} versions above 1.7.78.
#'
#' @details
#' Some palettes are mapped by default to a specific range of values (see
#' [grass_db]). Set `use_grass_range = FALSE` to map the color scales to the
#' range of values of the `fill/colour` aesthetics. See **Examples**.
#'
#' When passing the `limits` argument, the colors are restricted to those
#' specified by this argument, keeping the distribution of the palette. You can
#' combine this with `oob`, for example `oob = scales::oob_squish`, to avoid
#' blank pixels in the plot.
#'
#' @source
#'
#' Derived from <https://github.com/OSGeo/grass/tree/main/lib/gis/colors>. See
#' also [r.color - GRASS GIS
#' Manual](https://grass.osgeo.org/grass-stable/manuals/r.colors.html).
#'
#' @export
#' @encoding UTF-8
#'
#' @name scale_grass
#'
#' @seealso [grass_db], [terra::plot()],
#' [terra::minmax()], [ggplot2::scale_fill_viridis_c()].
#'
#' See also \CRANpkg{ggplot2} docs on additional `...` arguments:
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
#'   [grass_db] for more information.
#'
#' @param use_grass_range Logical. If `TRUE`, use the suggested range when
#'   plotting. See **Details**.
#' @returns
#' The corresponding \CRANpkg{ggplot2} layer with the values applied to the
#' `fill/colour` `aes()`.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::map.pal()]
#'
#' @references
#' GRASS Development Team (2024). *Geographic Resources Analysis Support System
#' (GRASS) Software, Version 8.3.2*. Open Source Geospatial Foundation, USA.
#' <https://grass.osgeo.org>.
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
#'   scale_fill_grass_d(palette = "soilmoisture")
#' }
scale_fill_grass_d <- function(
  palette = "viridis",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "fill",
    grass_pal(
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
#' @rdname scale_grass
scale_colour_grass_d <- function(
  palette = "viridis",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "colour",
    grass_pal(
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
#' @rdname scale_grass
#' @usage NULL
scale_color_grass_d <- scale_colour_grass_d

#' @export
#' @encoding UTF-8
#' @rdname scale_grass
scale_fill_grass_c <- function(
  palette = "viridis",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  use_grass_range = TRUE,
  na.value = "transparent",
  guide = "colourbar"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- grass_scale_params(
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    use_grass_range = use_grass_range
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
#' @rdname scale_grass
scale_colour_grass_c <- function(
  palette = "viridis",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  use_grass_range = TRUE,
  na.value = "transparent",
  guide = "colourbar"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- grass_scale_params(
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    use_grass_range = use_grass_range
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
#' @rdname scale_grass
#' @usage NULL
scale_color_grass_c <- scale_colour_grass_c

#' @export
#' @encoding UTF-8
#' @rdname scale_grass
scale_fill_grass_b <- function(
  palette = "viridis",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  use_grass_range = TRUE,
  na.value = "transparent",
  guide = "coloursteps"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- grass_scale_params(
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    use_grass_range = use_grass_range
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
#' @rdname scale_grass
scale_colour_grass_b <- function(
  palette = "viridis",
  ...,
  alpha = 1,
  direction = 1,
  values = NULL,
  limits = NULL,
  use_grass_range = TRUE,
  na.value = "transparent",
  guide = "coloursteps"
) {
  check_alpha_direction(alpha, direction)

  scale_params <- grass_scale_params(
    palette = palette,
    alpha = alpha,
    direction = direction,
    values = values,
    limits = limits,
    use_grass_range = use_grass_range
  )

  ggplot2::scale_color_stepsn(
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
#' @rdname scale_grass
#' @usage NULL
scale_color_grass_b <- scale_colour_grass_b

#' @export
#' @encoding UTF-8
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
grass.colors <- function(n, palette = "viridis", alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(tidyterra::grass_db, palette = palette)
    colors <- as.character(paltab$hex)
    if (anyNA(paltab$limit)) {
      endcols <- tidyterra_ramp(colors, n, alpha, rev)
    } else {
      endcols <- tidyterra_ramp2(
        colors,
        n,
        alpha,
        rev,
        limits = as.vector(paltab$limit)
      )
    }

    endcols
  } else {
    character()
  }
}

# Helpers ----
grass_scale_params <- function(
  palette,
  alpha,
  direction,
  values,
  limits,
  use_grass_range,
  call = rlang::caller_env()
) {
  coltab <- tidyterra::grass_db

  check_palette(palette, coltab$pal, help = "tidyterra::grass_db", call = call)

  pal_cols <- coltab[coltab$pal == palette, ]
  colors <- as.character(pal_cols$hex)
  if (direction == -1) {
    colors <- rev(colors)
  }
  if (alpha != 1) {
    colors <- ggplot2::alpha(colors, alpha = alpha)
  }

  if (any(!use_grass_range, anyNA(pal_cols$limit))) {
    rescaled_values <- values
    if (!is.null(values)) {
      rescaled_values <- scales::rescale(values)
    }
  } else {
    if (is.null(values)) {
      values <- pal_cols$limit
    }
    if (is.null(limits)) {
      limits <- range(values)
    }
    rescaled_values <- scales::rescale(values, from = limits)
  }

  list(
    colors = colors,
    values = rescaled_values,
    limits = limits
  )
}

grass_pal <- function(alpha = 1, direction = 1, palette) {
  function(n) {
    pal <- grass.colors(
      n,
      rev = direction != 1,
      alpha = alpha,
      palette = palette
    )

    pal
  }
}
