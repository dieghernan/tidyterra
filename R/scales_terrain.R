#' Terrain colour scales from grDevices
#'
#' @description
#'
#' Implementation of the classic color palettes used by default by the
#' terra package (see [terra::plot()]):
#'
#' - `scale_*_terrain_d()`: For discrete values.
#' - `scale_*_terrain_c()`: For continuous values.
#' - `scale_*_terrain_b()`: For binning continuous values.
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
#' @name scale_terrain
#'
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
#' @inheritParams scale_cross_blended
#'
#' @seealso
#' [terra::plot()], [ggplot2::scale_fill_viridis_c()] and \pkg{ggplot2} docs on
#' additional `...` parameters:
#'
#' - `scale_*_terrain_d()`: For discrete values.
#' - `scale_*_terrain_c()`: For continuous values.
#' - `scale_*_terrain_b()`: For binning continuous values.
#'
#'
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill/color` aesthetics.
#'
#' @family gradients
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_terrain_c()
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_terrain_b(breaks = seq(70, 200, 10))
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
#'
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_terrain_d(na.value = "gray10")
#' }
scale_fill_terrain_d <- function(..., alpha = 1, direction = 1,
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
    scale_name = "terrain_fill_d",
    palette = terrain_pal(
      alpha = alpha,
      direction = direction
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @rdname scale_terrain
scale_colour_terrain_d <- function(..., alpha = 1, direction = 1,
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
    scale_name = "terrain_colour_d",
    palette = terrain_pal(
      alpha = alpha,
      direction = direction
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}


#' @export
#' @rdname scale_terrain
scale_fill_terrain_c <- function(..., alpha = 1, direction = 1,
                                 na.value = "transparent",
                                 guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::continuous_scale(
    aesthetics = "fill",
    scale_name = "terrain_fill_c",
    scales::gradient_n_pal(terrain_pal(
      alpha = alpha,
      direction = direction
    )(100)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_terrain
scale_colour_terrain_c <- function(..., alpha = 1, direction = 1,
                                   na.value = "transparent",
                                   guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::continuous_scale(
    aesthetics = "colour",
    scale_name = "terrain_colour_c",
    scales::gradient_n_pal(terrain_pal(
      alpha = alpha,
      direction = direction
    )(100)),
    na.value = na.value,
    guide = guide,
    ...
  )
}


#' @export
#' @rdname scale_terrain
scale_fill_terrain_b <- function(..., alpha = 1, direction = 1,
                                 na.value = "transparent",
                                 guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::binned_scale(
    aesthetics = "fill",
    scale_name = "terrain_fill_c",
    scales::gradient_n_pal(terrain_pal(
      alpha = alpha,
      direction = direction
    )(100)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_terrain
scale_colour_terrain_b <- function(..., alpha = 1, direction = 1,
                                   na.value = "transparent",
                                   guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::binned_scale(
    aesthetics = "colour",
    scale_name = "terrain_colour_c",
    scales::gradient_n_pal(terrain_pal(
      alpha = alpha,
      direction = direction
    )(100)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

terrain_pal <- function(alpha = 1, direction = 1) {
  # nocov start
  function(n) {
    pal <- terrain.colors(n, rev = direction == 1, alpha = alpha)

    pal
  }
  # nocov end
}

#' @export
#' @rdname scale_terrain
#' @usage NULL
scale_color_terrain_d <- scale_colour_terrain_d


#' @export
#' @rdname scale_terrain
#' @usage NULL
scale_color_terrain_c <- scale_colour_terrain_c


#' @export
#' @rdname scale_terrain
#' @usage NULL
scale_color_terrain_b <- scale_colour_terrain_b
