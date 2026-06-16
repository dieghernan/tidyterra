#' Fortify `Spat*` objects
#'
#' @description
#' Fortify `SpatRaster` and `SpatVector` objects to data frames. This provides
#' native compatibility with [ggplot2::ggplot()].
#'
#' These methods are now implemented as wrappers around [`tidy.Spat`] methods.
#'
#' @export
#' @encoding UTF-8
#'
#' @rdname fortify.Spat
#' @name fortify.Spat
#'
#' @seealso [`tidy.Spat`], [sf::st_as_sf()], [`as_tibble.Spat`],
#'   [as_spatraster()], [ggplot2::fortify()].
#'
#' @family ggplot2.utils
#' @family ggplot2.methods
#' @family coerce
#'
#' @importFrom ggplot2 fortify
#' @inheritParams tidy.Spat
#' @param model A `SpatRaster` created with [terra::rast()], a `SpatVector`
#'   created with [terra::vect()], a `SpatGraticule` (see [terra::graticule()])
#'   or a `SpatExtent` (see [terra::ext()]).
#' @param data Not used by this method.
#' @returns
#'
#' [fortify.SpatVector()], [fortify.SpatGraticule()] and [fortify.SpatExtent()]
#' return a [`sf`][sf::st_sf] object.
#'
#' [fortify.SpatRaster()] returns a [tibble][tibble::tbl_df]. See **Methods**.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [ggplot2::fortify()] method.
#'
#' ## `SpatRaster`
#'
#' Returns a tibble that can be used with `ggplot2::geom_*`, such as
#' [ggplot2::geom_point()] and [ggplot2::geom_raster()].
#'
#' The resulting tibble includes coordinates in the `x` and `y` columns. The
#' values of each layer are added as extra columns using the layer names from
#' the `SpatRaster`.
#'
#' The CRS of the `SpatRaster` can be retrieved with
#' `attr(fortifiedSpatRaster, "crs")`.
#'
#' You can convert the fortified object back to a `SpatRaster` with
#' [as_spatraster()].
#'
#' When `pivot = TRUE`, the `SpatRaster` is fortified in long format (see
#' [tidyr::pivot_longer()]). The fortified object has the following columns:
#' - `x`, `y`: Coordinates of the cell center in the corresponding CRS.
#' - `lyr`: Name of the `SpatRaster` layer associated with `value`.
#' - `value`: Cell value for the corresponding `lyr`.
#'
#' This option can be useful when combining several `geom_*` layers or when
#' faceting.
#'
#' ## `SpatVector`, `SpatGraticule` and `SpatExtent`
#'
#' Returns an [`sf`][sf::st_sf] object that can be used with
#' [ggplot2::geom_sf()].
#'
#' @examples
#' \donttest{
#'
#' # Demonstrate use with ggplot2.
#' library(ggplot2)
#'
#' # Get a SpatRaster.
#' r <- system.file("extdata/volcano2.tif", package = "tidyterra") |>
#'   terra::rast() |>
#'   terra::project("EPSG:4326")
#'
#' # You can now use a SpatRaster with any geom.
#' ggplot(r, maxcell = 50) +
#'   geom_histogram(aes(x = elevation),
#'     bins = 20, fill = "lightblue",
#'     color = "black"
#'   )
#'
#' # For SpatVector, SpatGraticule and SpatExtent, use geom_sf().
#'
#' # Create a SpatVector.
#' extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#' cyl <- terra::vect(extfile)
#'
#' class(cyl)
#'
#' ggplot(cyl) +
#'   geom_sf()
#'
#' # SpatGraticule
#' g <- terra::graticule(60, 30, crs = "+proj=robin")
#'
#' class(g)
#'
#' ggplot(g) +
#'   geom_sf()
#'
#' # SpatExtent
#' ex <- terra::ext(cyl)
#'
#' class(ex)
#'
#' ggplot(ex, crs = cyl) +
#'   geom_sf(fill = "red", alpha = 0.3) +
#'   geom_sf(data = cyl, fill = NA)
#' }
#'
fortify.SpatRaster <- function(
  model,
  data,
  ...,
  .name_repair = c(
    "unique",
    "check_unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  ),
  maxcell = terra::ncell(model) * 1.1,
  pivot = FALSE
) {
  tidy(
    x = model,
    ...,
    .name_repair = .name_repair,
    maxcell = maxcell,
    pivot = pivot
  )
}

#' @export
#' @encoding UTF-8
#' @name fortify.Spat
fortify.SpatVector <- function(model, data, ...) {
  tidy(x = model, ...)
}

#' @export
#' @encoding UTF-8
#' @name fortify.Spat
fortify.SpatGraticule <- function(model, data, ...) {
  tidy(x = model, ...)
}

#' @export
#' @encoding UTF-8
#' @name fortify.Spat
fortify.SpatExtent <- function(model, data, ..., crs = "") {
  tidy(x = model, ..., crs = crs)
}

#' @export
ggplot2::fortify
