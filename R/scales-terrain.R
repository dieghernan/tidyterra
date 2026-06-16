#' Terrain color scales from \pkg{grDevices}
#'
#' @description
#'
#' Implementation of the classic color palette [terrain.colors()]:
#'
#' - `scale_*_terrain_d()`: For discrete values.
#' - `scale_*_terrain_c()`: For continuous values.
#' - `scale_*_terrain_b()`: For binning continuous values.
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
#' @export
#' @encoding UTF-8
#'
#' @name scale_terrain
#'
#' @seealso
#' [terra::plot()], [ggplot2::scale_fill_viridis_c()] and \CRANpkg{ggplot2} docs
#' on additional `...` arguments.
#'
#' @family gradients
#'
#' @inheritParams scale_cross_blended
#'
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
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
#' factor <- volcano2_rast |> mutate(cats = cut(elevation,
#'   breaks = c(100, 120, 130, 150, 170, 200),
#'   labels = c(
#'     "Very Low", "Low", "Average", "High",
#'     "Very High"
#'   )
#' ))
#'
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_terrain_d(na.value = "gray10")
#' }
scale_fill_terrain_d <- function(
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "fill",
    terrain_pal(alpha = alpha, direction = direction),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
scale_colour_terrain_d <- function(
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "colour",
    terrain_pal(alpha = alpha, direction = direction),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
scale_fill_terrain_c <- function(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  pal_gradient_scale(
    ggplot2::continuous_scale,
    "fill",
    terrain_pal(alpha = alpha, direction = direction),
    n = 100,
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
scale_colour_terrain_c <- function(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  pal_gradient_scale(
    ggplot2::continuous_scale,
    "colour",
    terrain_pal(alpha = alpha, direction = direction),
    n = 100,
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
scale_fill_terrain_b <- function(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  pal_gradient_scale(
    ggplot2::binned_scale,
    "fill",
    terrain_pal(alpha = alpha, direction = direction),
    n = 100,
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
scale_colour_terrain_b <- function(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  pal_gradient_scale(
    ggplot2::binned_scale,
    "colour",
    terrain_pal(alpha = alpha, direction = direction),
    n = 100,
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

terrain_pal <- function(alpha = 1, direction = 1) {
  function(n) {
    pal <- terrain.colors(n, rev = direction == 1, alpha = alpha)

    pal
  }
}

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
#' @usage NULL
scale_color_terrain_d <- scale_colour_terrain_d

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
#' @usage NULL
scale_color_terrain_c <- scale_colour_terrain_c

#' @export
#' @encoding UTF-8
#' @rdname scale_terrain
#' @usage NULL
scale_color_terrain_b <- scale_colour_terrain_b
