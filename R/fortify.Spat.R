#' Fortify Spat* Objects
#'
#' Fortify SpatRasters and SpatVectors to data frames for compatibility with
#' [ggplot2::ggplot()].
#'
#'
#' @param model A SpatRaster created with [terra::rast()] or a SpatVector
#'   created with [terra::vect()].
#' @param data Not used by this method.
#' @param maxcell positive integer. Maximum number of cells to use for the plot.
#' @inheritParams ggplot2::fortify
#' @inheritParams as_tibble.Spat
#' @importFrom ggplot2 fortify
#' @export
#'
#' @family ggplot2.utils
#' @family ggplot2.methods
#' @family coerce
#'
#' @return [fortify.SpatVector()] returns a `sf` object and
#'   [fortify.SpatVector()] returns a tibble. See **Methods**.
#'
#' @rdname fortify.Spat
#' @name fortify.Spat
#'
#' @seealso [sf::st_as_sf()], [as_tibble.Spat], [as_spatraster()],
#'   [ggplot2::fortify()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [ggplot2::fortify()] function.
#'
#' ## SpatRaster
#'
#' Return a tibble than can be used with `ggplot2::geom_*` like
#' [ggplot2::geom_point()], [ggplot2::geom_raster()], etc.
#'
#' The resulting tibble includes the coordinates on the columns `x,y`. The
#' values of each layer are included as additional columns named as per the
#' name of the layer on the SpatRaster.
#'
#' The CRS of the SpatRaster can be retrieved with
#' `attr(<fortifiedSpatRaster>, "crs")`.
#'
#' It is possible to convert the fortified object onto a SpatRaster again with
#' [as_spatraster()].
#'
#' ## SpatVector
#'
#' Return a `sf` object than can be used with [ggplot2::geom_sf()].
#'
#' @examples
#' \donttest{
#'
#' # Get a SpatRaster
#' r <- system.file("extdata/volcano2.tif", package = "tidyterra") %>%
#'   terra::rast()
#'
#' fortified <- ggplot2::fortify(r)
#'
#' fortified
#'
#' # The crs is an attribute of the fortified SpatRaster
#'
#' attr(fortified, "crs")
#'
#' # Back to a SpatRaster with
#' as_spatraster(fortified)
#'
#' # You can now use a SpatRaster with raster, contours, etc.
#' library(ggplot2)
#'
#' # Use here the raster with resample
#' ggplot(r, maxcell = 10000) +
#'   # Need the aes parameters
#'   geom_raster(aes(x, y, fill = elevation)) +
#'   # Adjust the coords
#'   coord_equal()
#'
#' # Or any other geom
#' ggplot(r) +
#'   geom_histogram(aes(x = elevation), bins = 20, fill = "lightblue", color = "black")
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
fortify.SpatRaster <- function(model, data, ..., .name_repair = "unique",
                               maxcell = terra::ncell(model) * 1.1) {
  model <- resample_spat(model, maxcell)

  crs <- pull_crs(model)

  if (is.na(crs)) crs <- ""

  model <- as_tibble(model, xy = TRUE, .name_repair = .name_repair)
  attr(model, "crs") <- crs

  model
}

#' @export
#' @name fortify.Spat
fortify.SpatVector <- function(model, data, ...) {
  sf::st_as_sf(model)
}




#' @export
ggplot2::fortify
