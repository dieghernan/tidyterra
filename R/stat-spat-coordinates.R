#' Extract coordinates from [`SpatVector`][terra::vect] objects
#'
#' `stat_spat_coordinates()` extracts the coordinates from
#'   [`SpatVector`][terra::vect] objects
#' and summarizes them to one pair of coordinates (x and y) per geometry.
#'
#' Wrapper of [ggplot2::stat_sf_coordinates()].
#'
#' @details
#'
#' See [ggplot2::stat_sf_coordinates()] for details.
#'
#' @rdname stat_spat_coordinates
#'
#' @keywords internal
#'
#' @family ggplot2.utils
#' @inheritParams ggspatvector
#' @inheritParams ggplot2::stat_sf_coordinates
#' @param ... Other arguments passed to [ggplot2::stat_sf_coordinates()].
#'
#' @returns A \CRANpkg{ggplot2} layer.
#' @encoding UTF-8
#' @export
#' @examples
#' \donttest{
#' cyl <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' library(ggplot2)
#'
#' ggplot(cyl) +
#'   stat_spat_coordinates()
#'
#' ggplot(cyl) +
#'   geom_errorbar(
#'     aes(
#'       geometry = geometry,
#'       xmin = after_stat(x) - 50000,
#'       xmax = after_stat(x) + 50000,
#'       y = after_stat(y)
#'     ),
#'     stat = "sf_coordinates",
#'     orientation = "y",
#'     width = 10000
#'   )
#' }
#'
stat_spat_coordinates <- function(
  mapping = aes(),
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  ggplot2::stat_sf_coordinates(
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    ...
  )
}
