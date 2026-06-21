#' Determine packages required by `Spat*` objects
#'
#' Determine packages required by `Spat*` objects.
#'
#' @rdname required_pkgs.Spat
#' @name required_pkgs.Spat
#'
#' @seealso [generics::required_pkgs()].
#'
#' @family generics.methods
#'
#' @importFrom generics required_pkgs
#'
#' @inheritParams tidy.Spat
#'
#' @returns A character string of packages that are required.
#' @section Methods:
#'
#' Implementation of the **generic** [generics::required_pkgs()] method for
#' `Spat*` objects.
#'
#' @encoding UTF-8
#' @export
#' @examples
#' file_path <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#'
#' library(terra)
#'
#' r <- rast(file_path)
#'
#' # With rasters
#' r
#' required_pkgs(r)
#'
#' # With vectors
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v
#' required_pkgs(v)
#'
required_pkgs.SpatRaster <- function(x, ...) {
  c("terra")
}

#' @rdname required_pkgs.Spat
#' @export
required_pkgs.SpatVector <- function(x, ...) {
  c("terra")
}

#' @rdname required_pkgs.Spat
#' @export
required_pkgs.SpatGraticule <- function(x, ...) {
  c("terra")
}

#' @rdname required_pkgs.Spat
#' @export
required_pkgs.SpatExtent <- function(x, ...) {
  c("terra")
}
#' @export
generics::required_pkgs
