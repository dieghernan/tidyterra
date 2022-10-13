#' Fortify Spat Objects
#'
#' Fortify SpatVectors and SpatRasters to data frames for compatibility with
#' [ggplot2::ggplot()].
#'
#'
#' @param model A SpatVector [terra::vect()].
#' @param data Not used by this method.
#' @inheritParams ggplot2::fortify
#' @importFrom ggplot2 fortify
#' @export
#'
#' @family ggplot2.utils
#' @family ggplot2.methods
#'
#' @return [fortify.SpatVector()] returns a `sf` object. See **Methods**.
#'
#' @rdname fortify.Spat
#' @name fortify.Spat
#'
#'
#' @section Methods:
#'
#' Implementation of the **generic** [ggplot2::fortify()] function.
#'
#'
#' ## SpatVector
#'
#' Return a `sf` object.
#'
#' @examples
#' \donttest{
#'
#' # Create a SpatVector
#' extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#' cyl <- terra::vect(extfile)
#'
#' cyl
#'
#' # To sf
#' ggplot2::fortify(cyl)
#'
#' # Now you can use geom_sf()
#'
#' library(ggplot2)
#'
#' ggplot(cyl) +
#'   geom_sf()
#' }
#'
fortify.SpatVector <- function(model, data, ...) {
  sf::st_as_sf(model)
}

# TODO
# fortify.SpatRaster <- function(model, data, ..., maxcell = 500000) {
#   model <- resample_spat(model, maxcell)
#
#   as_tibble.SpatRaster(model, xy = TRUE)
# }


#' @export
ggplot2::fortify
