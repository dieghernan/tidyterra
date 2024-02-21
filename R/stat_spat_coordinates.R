#' Extract coordinates from SpatVector objects
#'
#' `stat_spat_coordinates()` extracts the coordinates from SpatVector objects
#' and summarises them to one pair of coordinates (x and y) per geometry.
#'
#' Wrapper of [ggplot2::stat_sf_coordinates()].
#'
#'
#' @rdname stat_spat_coordinates
#'
#' @export
#' @family ggplot2.utils
#' @return A \CRANpkg{ggplot2} layer
#' @keywords internal
#'
#'
#' @inheritParams ggspatvector
#' @inheritParams ggplot2::stat_sf_coordinates
#' @param ... Other arguments passed on to [ggplot2::stat_sf_coordinates()].
#'
#' @details
#'
#' See [ggplot2::stat_sf_coordinates()] for details.
#'
#'
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
#'   geom_errorbarh(
#'     aes(
#'       geometry = geometry,
#'       xmin = after_stat(x) - 50000,
#'       xmax = after_stat(x) + 50000,
#'       y = after_stat(y),
#'       height = 10000
#'     ),
#'     stat = "sf_coordinates"
#'   )
#' }
#'
stat_spat_coordinates <- function(mapping = aes(), data = NULL, geom = "point",
                                  position = "identity", na.rm = FALSE,
                                  show.legend = NA, inherit.aes = TRUE,
                                  ...) {
  # nocov start
  ggplot2::stat_sf_coordinates(
    mapping = mapping, data = data, geom = geom,
    position = position, na.rm = na.rm,
    show.legend = show.legend, inherit.aes = inherit.aes,
    ...
  )
  # nocov end
}
