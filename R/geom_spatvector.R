#' Visualise SpatVector objects
#'
#' @description
#'
#' This geom is used to visualise SpatVector objects (see [terra::vect()]). For
#' simple plots, you will only need `geom_spatvector()`. For text and labels,
#' you can use `geom_spatvector_text()` and` geom_spatvector_label()`.
#'
#'
#' The underlying implementation is based on [ggplot2::geom_sf()].
#'
#' @return A ggplot2 layer
#' @family ggplot2.utils
#' @param data A SpatRaster object.
#'
#' @name ggspatvector
#'
#' @param data A SpatVector object, see [terra::vect()].
#'
#' @param ... Other arguments passed on to [ggplot2::geom_sf()]. These are
#'    often aesthetics, used to set an aesthetic to a fixed value, like
#'    `colour = "red"` or `size = 3`.
#'
#' @inheritParams ggplot2::geom_sf
#'
#' @seealso [ggplot2::geom_sf()]
#'
#' @details
#'
#' See [ggplot2::geom_sf()] for details on aesthetics, etc.
#'
#' @section  terra equivalent:
#'
#' [terra::plot()]
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a SpatVector
#'
#' extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#'
#' cyl <- terra::vect(extfile)
#' class(cyl)
#' #' # Create a SpatVector
#'
#' extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#'
#' cyl <- terra::vect(extfile)
#' class(cyl)
#'
#' library(ggplot2)
#'
#' ggplot() +
#'   geom_spatvector(data = cyl)
#'
#' # Avoid this!
#' if (FALSE) {
#'   # Would produce an error
#'
#'   ggplot(cyl) +
#'     geom_spatvector()
#' }
#' # With params
#'
#' ggplot() +
#'   geom_spatvector(data = cyl, aes(fill = name), color = NA) +
#'   scale_fill_viridis_d() +
#'   coord_sf(crs = 3857)
#'
#' # Add labels
#' ggplot() +
#'   geom_spatvector(data = cyl, aes(fill = name), color = NA) +
#'   geom_spatvector_text(
#'     data = cyl, aes(label = iso2), fontface = "bold", size = 3,
#'     color = "red"
#'   ) +
#'   scale_fill_viridis_d(alpha = 0.4) +
#'   coord_sf(crs = 3857)
#' }
geom_spatvector <- function(mapping = aes(),
                            data,
                            na.rm = FALSE,
                            show.legend = NA,
                            ...) {
  ggplot2::geom_sf(
    data = sf::st_as_sf(data),
    mapping = mapping,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = FALSE,
    ...
  )
}

#' @export
#' @name ggspatvector
geom_spatvector_label <- function(mapping = aes(),
                                  data,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  ...) {
  ggplot2::geom_sf_label(
    data = sf::st_as_sf(data),
    mapping = mapping,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = FALSE,
    ...
  )
}


#' @export
#' @name ggspatvector
geom_spatvector_text <- function(mapping = aes(),
                                 data,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 ...) {
  ggplot2::geom_sf_text(
    data = sf::st_as_sf(data),
    mapping = mapping,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = FALSE,
    ...
  )
}
