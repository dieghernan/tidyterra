#' Turn `Spat*` object into a tidy tibble
#'
#' Turn `Spat*` object into a tidy tibble. This is similar to
#' [`fortify.Spat`], and it is provided just in case [ggplot2::fortify()]
#' method is deprecated in the future.
#'
#' @param x A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()]. Also support `SpatGraticule` (see
#'   [terra::graticule()]) and `SpatExtent` (see [terra::ext()]).
#' @param ... Ignored by these methods.
#' @param pivot Logical. When `TRUE` the `SpatRaster` would be provided on
#'   [long format][tidyr::pivot_longer()]. When `FALSE` (the default) it would
#'   be provided as a data frame with a column for each layer. See **Details**.
#' @inheritParams geom_spatraster
#' @inheritParams as_tibble.Spat
#' @inheritParams generics::tidy
#'
#'
#' @importFrom generics tidy
#' @export
#'
#' @family generics.methods
#' @family coerce
#'
#' @return
#'
#' [`tidy.SpatVector()`], [`tidy.SpatGraticule()`] and [`tidy.SpatExtent()`]
#' return a [`sf`][sf::st_sf] object.
#'
#' [`tidy.SpatRaster()`] returns a [`tibble`][tibble::tibble]. See **Methods**.
#'
#' @rdname tidy.Spat
#' @name tidy.Spat
#'
#' @seealso
#' [sf::st_as_sf()], [`as_tibble.Spat`], [as_spatraster()], [`fortify.Spat`],
#' [ggplot2::fortify()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [generics::tidy()] method.
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
#' `attr(tidySpatRaster, "crs")`.
#'
#' It is possible to convert the tidy object onto a `SpatRaster` again with
#' [as_spatraster()].
#'
#' When `pivot = TRUE` the `SpatRaster` is provided in a "long" format (see
#' [tidyr::pivot_longer()]). The tidy object would have the following
#' columns:
#' - `x,y`: Coordinates (center) of the cell on the corresponding CRS.
#' - `lyr`: Indicating the name of the `SpatRaster` layer of `value`.
#' - `value`: The value of the `SpatRaster` in the corresponding `lyr`.
#'
#' This option may be useful when using several `geom_*` and for faceting.
#'
#' ## `SpatVector`, `SpatGraticule` and `SpatExtent`
#'
#' Return a [`sf`][sf::st_sf] object than can be used with [ggplot2::geom_sf()].
#'
#' @examples
#' \donttest{
#'
#' # Get a SpatRaster
#' r <- system.file("extdata/volcano2.tif", package = "tidyterra") %>%
#'   terra::rast() %>%
#'   terra::project("EPSG:4326")
#'
#' r_tidy <- tidy(r)
#'
#' r_tidy
#'
#' # Back to a SpatRaster with
#' as_spatraster(r_tidy)
#'
#' # SpatVector
#' cyl <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' cyl
#'
#' tidy(cyl)
#'
#' # SpatExtent
#' ex <- cyl %>% terra::ext()
#'
#' ex
#'
#' tidy(ex)
#'
#' # With crs
#' tidy(ex, crs = pull_crs(cyl))
#'
#' # SpatGraticule
#' grat <- terra::graticule(60, 30, crs = "+proj=robin")
#'
#' grat
#' tidy(grat)
#' }
tidy.SpatRaster <- function(x, ..., .name_repair = "unique",
                            maxcell = terra::ncell(x) * 1.1,
                            pivot = FALSE) {
  x <- resample_spat(x, maxcell)

  crs <- pull_crs(x)

  if (is.na(crs)) {
    crs <- ""
  }

  if (pivot == FALSE) {
    x <- as_tibble(x, xy = TRUE, .name_repair = .name_repair)
  } else {
    x <- check_mixed_cols(x, fn = "tidyterra::tidy.SpatRaster")
    x <- pivot_longer_spat(x)
    attr(x, "pvt_fort") <- TRUE
  }

  attr(x, "crs") <- crs

  x
}

#' @export
#' @name tidy.Spat
tidy.SpatVector <- function(x, ...) {
  as_sf(x)
}

#' @export
#' @name tidy.Spat
tidy.SpatGraticule <- function(x, ...) {
  # nocov start
  tvers <- packageVersion("terra")
  if (tvers < "1.8.5") {
    msg <- paste(
      "Need {.pkg terra} {.strong 1.8.5} or later for ",
      "{.fn tidy.SpatGraticule} method. Current {.pkg terra} ",
      "version is  {.strong {tvers}}"
    )
    cli::cli_abort(msg)
  }
  # nocov end
  as_sf(terra::vect(x))
}

#' @export
#' @name tidy.Spat
#' @param crs Input potentially including or representing a CRS. It could be
#'   a `sf/sfc` object, a `SpatRaster/SpatVector` object, a `crs` object from
#'   [sf::st_crs()], a character (for example a [proj4
#'   string](https://proj.org/en/9.3/operations/projections/index.html)) or a
#'   integer (representing an [EPSG](https://epsg.io/) code).
tidy.SpatExtent <- function(x, ..., crs = "") {
  as_sf(terra::vect(x, crs = pull_crs(crs)))
}

#' @export
generics::tidy
