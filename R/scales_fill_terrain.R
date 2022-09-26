#' Terrain colour fill scales from grDevices
#'
#' @description
#'
#' Implementation of the classic color palettes used by default by the
#' terra package (see [terra::plot()]). Three fill scales are provided:
#'
#' - `scale_fill_terrain_d()`: For discrete values.
#' - `scale_fill_terrain_c()`: For continuous values.
#' - `scale_fill_terrain_b()`: For binning continuous values.
#'
#'
#' @export
#'
#' @name scale_fill_terrain
#'
#' @inheritParams ggplot2::scale_fill_viridis_b
#'
#' @seealso [terra::plot()], [ggplot2::scale_fill_viridis_c()]
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill` aesthetics.
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
scale_fill_terrain_d <- function(..., alpha = 1, direction = 1) {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "terrain_fill_d",
    palette = terrain_pal(
      alpha = alpha,
      direction = direction
    ),
    ...
  )
}

#' @export
#' @rdname scale_fill_terrain
scale_fill_terrain_c <- function(..., alpha = 1, direction = 1,
                                 na.value = NA, guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

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
#' @rdname scale_fill_terrain
scale_fill_terrain_b <- function(..., alpha = 1, direction = 1,
                                 na.value = NA, guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

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

terrain_pal <- function(alpha = 1, direction = 1) {
  # nocov start
  function(n) {
    pal <- terrain.colors(n, rev = direction == 1, alpha = alpha)

    pal
  }
  # nocov end
}
