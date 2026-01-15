#' Fortify `Spat*` Objects
#'
#' @description
#' Fortify `SpatRaster` and `SpatVector` objects to data frames. This provide
#' native compatibility with [ggplot2::ggplot()].
#'
#' **Note that** these methods are now implemented as a wrapper of [`tidy.Spat`]
#' methods.
#'
#'
#' @param model A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()]. Also support `SpatGraticule` (see
#'   [terra::graticule()]) and `SpatExtent` (see [terra::ext()]).
#' @param data Not used by this method.
#' @inheritParams tidy.Spat
#' @importFrom ggplot2 fortify
#' @export
#'
#' @family ggplot2.utils
#' @family ggplot2.methods
#' @family coerce
#'
#' @return
#'
#' [fortify.SpatVector()], [fortify.SpatGraticule()] and [fortify.SpatExtent()]
#' return a [`sf`][sf::st_sf] object.
#'
#' [fortify.SpatRaster()] returns a [tibble][tibble::tbl_df]. See **Methods**.
#'
#' @rdname fortify.Spat
#' @name fortify.Spat
#'
#' @seealso [`tidy.Spat`], [sf::st_as_sf()], [`as_tibble.Spat`],
#'   [as_spatraster()], [ggplot2::fortify()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [ggplot2::fortify()] method.
#'
#' ## `SpatRaster`
#'
#' Return a tibble than can be used with `ggplot2::geom_*` like
#' [ggplot2::geom_point()], [ggplot2::geom_raster()], etc.
#'
#' The resulting tibble includes the coordinates on the columns `x, y`. The
#' values of each layer are included as additional columns named as per the
#' name of the layer on the `SpatRaster`.
#'
#' The CRS of the `SpatRaster` can be retrieved with
#' `attr(fortifiedSpatRaster, "crs")`.
#'
#' It is possible to convert the fortified object onto a `SpatRaster` again with
#' [as_spatraster()].
#'
#' When `pivot = TRUE` the `SpatRaster` is fortified in a "long" format (see
#' [tidyr::pivot_longer()]). The fortified object would have the following
#' columns:
#' - `x,y`: Coordinates (center) of the cell on the corresponding CRS.
#' - `lyr`: Indicating the name of the `SpatRaster` layer of `value`.
#' - `value`: The value of the `SpatRaster` in the corresponding `lyr`.
#'
#' This option may be useful when using several `geom_*` and for faceting, see
#' **Examples**.
#'
#' ## `SpatVector`, `SpatGraticule` and `SpatExtent`
#'
#' Return a [`sf`][sf::st_sf] object than can be used with [ggplot2::geom_sf()].
#'
#' @examples
#' \donttest{
#'
#' # Demonstrate the use with ggplot2
#' library(ggplot2)
#'
#'
#' # Get a SpatRaster
#' r <- system.file("extdata/volcano2.tif", package = "tidyterra") |>
#'   terra::rast() |>
#'   terra::project("EPSG:4326")
#'
#'
#' # You can now use a SpatRaster with any geom
#' ggplot(r, maxcell = 50) +
#'   geom_histogram(aes(x = elevation),
#'     bins = 20, fill = "lightblue",
#'     color = "black"
#'   )
#'
#' # For SpatVector, SpatGraticule and SpatExtent you can use now geom_sf()
#'
#' # Create a SpatVector
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
  .name_repair = "unique",
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
#' @name fortify.Spat
fortify.SpatVector <- function(model, data, ...) {
  tidy(x = model, ...)
}

#' @export
#' @name fortify.Spat
fortify.SpatGraticule <- function(model, data, ...) {
  tidy(x = model, ...)
}

#' @export
#' @name fortify.Spat
fortify.SpatExtent <- function(model, data, ..., crs = "") {
  tidy(x = model, ..., crs = crs)
}

#' @export
ggplot2::fortify
