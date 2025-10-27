#' Determine packages required by `Spat*` objects
#'
#' Determine packages required by `Spat*` objects.
#'
#' Implementation of [generics::required_pkgs()] method.
#'
#' @return A character string of packages that are required.
#' @family generics.methods
#'
#' @param x A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()]. Also support `SpatGraticule`
#'   (see [terra::graticule()]) and `SpatExtent` (see [terra::ext()]).
#' @param ... Ignored by these methods.
#'
#' @rdname required_pkgs.Spat
#' @name required_pkgs.Spat
#'
#'
#'
#' @export
#' @importFrom generics required_pkgs
#'
#' @seealso [generics::required_pkgs()]
#'
#' @examples
#' file_path <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#'
#'
#' library(terra)
#'
#' r <- rast(file_path)
#'
#' # With rasters
#' r
#' required_pkgs(r)
#'
#' #  With vectors
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v
#' required_pkgs(v)
#'
required_pkgs.SpatRaster <- function(x, ...) {
  c("terra")
}

#' @export
#' @name required_pkgs.Spat
required_pkgs.SpatVector <- function(x, ...) {
  c("terra")
}

#' @export
#' @name required_pkgs.Spat
required_pkgs.SpatGraticule <- function(x, ...) {
  c("terra")
}

#' @export
#' @name required_pkgs.Spat
required_pkgs.SpatExtent <- function(x, ...) {
  c("terra")
}
#' @export
generics::required_pkgs
