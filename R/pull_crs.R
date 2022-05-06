#' Extract CRS on WKT format
#'
#' @description
#'
#' Extract the WKT version of the CRS associated to a string, number of
#' sf/Spat* object.
#'
#' The [Well-known text (WKT)](https://en.wikipedia.org/wiki/Well-known_text_representation_of_coordinate_reference_systems)
#' representation of coordinate reference systems (CRS) is a character string
#' that identifies precisely the parameters of each CRS. This is the current
#' standard used on sf and terra packages.
#'
#' @seealso [terra::crs()], [sf::st_crs()]
#'
#' @family helpers
#' @export
#'
#' @param .data Input potentially including or representing a CRS. It could be
#'   a `sf/sfc` object, a `SpatRaster/SpatVector` object, a `crs` object from
#'   [sf::st_crs()], a character (for example a proj4 string) or a integer
#'   (representing an EPSG code).
#'
#' @param ... ignored
#'
#' @details
#'
#' Although the WKT representation is the same, sf and terra slightly differs.
#' For example, a sf user could do:
#'
#' `sf::st_transform(x, 25830)`
#'
#' While a terra user needs to:
#'
#' `terra::project(bb, "epsg:25830")`
#'
#' Knowing the WKT would help to smooth workflows when working with different
#' packages and object types.
#'
#' @section Internals:
#'
#' This is a thin wrapper of [sf::st_crs()] and [terra::crs()].
#' @examples
#'
#' # sf objects
#'
#' sfobj <- sf::st_as_sfc("MULTIPOINT ((0 0), (1 1))", crs = 4326)
#'
#' fromsf1 <- pull_crs(sfobj)
#' fromsf2 <- pull_crs(sf::st_crs(sfobj))
#'
#' # terra
#'
#' v <- terra::vect(sfobj)
#' r <- terra::rast(v)
#'
#' fromterra1 <- pull_crs(v)
#' fromterra2 <- pull_crs(r)
#'
#' # integers
#' fromint <- pull_crs(4326)
#'
#' # Characters
#' fromchar <- pull_crs("epsg:4326")
#'
#'
#' all(
#'   fromsf1 == fromsf2,
#'   fromsf2 == fromterra1,
#'   fromterra1 == fromterra2,
#'   fromterra2 == fromint,
#'   fromint == fromchar
#' )
#'
#' cat(fromsf1)
pull_crs <- function(.data, ...) {

  # Spat* objects handled by crs
  if (any(
    inherits(.data, "SpatRaster"),
    inherits(.data, "SpatVector")
  )) {
    .data <- terra::crs(.data)
  }

  if (any(
    is.na(.data), is.null(.data)
  )) {
    return(NA)
  }

  if (inherits(.data, "character")) {
    if (.data == "") {
      return(NA)
    }
  }

  # sf objects, characters and numerics are handled by sf
  if (any(
    inherits(.data, "sf"),
    inherits(.data, "sfc"),
    inherits(.data, "crs"),
    inherits(.data, "character"),
    inherits(.data, "numeric")
  )) {
    return(sf::st_crs(.data)$wkt)
  }

  # All the rest, return empty with an alert

  cli::cli_alert_warning("No wkt equivalent found. Returning NA")
  return(NA)
}
