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
#' @family ggplot2.utils
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#'
#'
#' library(terra)
#' cyl_temp <- rast(filepath)
#'
#' # Modify with tidyverse methods
#' library(dplyr)
#' continous <- cyl_temp %>% select(tavg_05)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = continous) +
#'   scale_fill_terrain_c()
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = continous) +
#'   scale_fill_terrain_b(breaks = seq(5, 20, 2.5))
#'
#' # With discrete values
#' factor <- continous %>% mutate(tavg_05 = cut(tavg_05,
#'   breaks = c(0, 7, 9, 11, 13, 15),
#'   labels = c(
#'     "Very Cold", "Cold", "Mild", "Hot",
#'     "Very Hot"
#'   )
#' ))
#'
#'
#'
#'
#' ggplot() +
#'   geom_spatraster(data = factor) +
#'   scale_fill_terrain_d(na.value = "gray80")
#' }
scale_fill_terrain_d <- function(..., alpha = 1, direction = 1) {
  if (alpha < 0 | alpha > 1) {
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
  if (alpha < 0 | alpha > 1) {
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
  if (alpha < 0 | alpha > 1) {
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
