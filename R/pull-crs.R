#' Extract CRS in WKT format
#'
#' @description
#'
#' Extract the WKT version of the CRS associated with a string, number or
#' `sf/Spat*` object.
#'
#' ```{r, echo=FALSE, results='asis'}
#' full_url <- paste0(
#'   "[Well-known text (WKT)](",
#'   "https://en.wikipedia.org/wiki/Well-known_text_",
#'   "representation_of_coordinate_reference_systems)"
#' )
#'
#' cat(full_url)
#' ```
#' is a character string representation of coordinate reference systems (CRS).
#' It identifies the parameters of each CRS precisely and is the standard used
#' by \CRANpkg{sf} and \CRANpkg{terra}.
#'
#' @details
#'
#' Although the WKT representation is the same, the \CRANpkg{sf} and
#' \CRANpkg{terra} APIs differ slightly. For example, \CRANpkg{sf} can do:
#'
#' `sf::st_transform(x, 25830)`
#'
#' While the \CRANpkg{terra} equivalent is:
#'
#' `terra::project(bb, "epsg:25830")`
#'
#' Knowing the WKT helps smooth workflows when working with different
#' packages and object types.
#'
#' @export
#' @encoding UTF-8
#'
#' @seealso
#'
#' [terra::crs()] and [sf::st_crs()] to learn how these packages handle
#' CRS definitions.
#'
#' @family helpers
#' @param .data Input potentially including or representing a CRS. It could be
#'   a `sf/sfc` object, a `SpatRaster/SpatVector` object, a `crs` object from
#'   [sf::st_crs()], a character (for example a [proj4
#'   string](https://proj.org/en/9.3/operations/projections/index.html)) or a
#'   integer (representing an [EPSG](https://epsg.io/) code).
#'
#' @param ... Ignored.
#'
#' @returns A WKT representation of the corresponding CRS.
#'
#' @section Internals:
#'
#' A thin wrapper around [sf::st_crs()] and [terra::crs()].
#'
#' @examples
#'
#' # sf objects.
#'
#' sfobj <- sf::st_as_sfc("MULTIPOINT ((0 0), (1 1))", crs = 4326)
#'
#' fromsf1 <- pull_crs(sfobj)
#' fromsf2 <- pull_crs(sf::st_crs(sfobj))
#'
#' # terra objects.
#'
#' v <- terra::vect(sfobj)
#' r <- terra::rast(v)
#'
#' fromterra1 <- pull_crs(v)
#' fromterra2 <- pull_crs(r)
#'
#' # Integers.
#' fromint <- pull_crs(4326)
#'
#' # Characters.
#' fromchar <- pull_crs("epsg:4326")
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
  # Spat* objects are handled by CRS.
  if (any(inherits(.data, "SpatRaster"), inherits(.data, "SpatVector"))) {
    .data <- terra::crs(.data)
  }

  if (
    any(inherits(.data, "sf"), inherits(.data, "sfc"), inherits(.data, "crs"))
  ) {
    return(sf::st_crs(.data)$wkt)
  }

  if (anyNA(.data)) {
    return(NA)
  }

  if (is.null(.data)) {
    return(NA)
  }

  if ((inherits(.data, "character")) && (!nzchar(.data))) {
    return(NA)
  }

  # Characters and numerics are handled by sf.
  if (any(inherits(.data, "character"), inherits(.data, "numeric"))) {
    return(sf::st_crs(.data)$wkt)
  }

  # Return `NA` with an alert for all other inputs.
  cli::cli_alert_warning(paste(
    "{.fun tidyterra::pull_crs} could not find a WKT equivalent.",
    "Returning {.val {NA}}."
  ))
  NA
}
