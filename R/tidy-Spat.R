#' Tidy `Spat*` objects for plotting
#'
#' `tidy()` methods for `SpatRaster`, `SpatVector`, `SpatGraticule` and
#' `SpatExtent` objects. These methods return a tibble for `SpatRaster`
#' objects and `sf` objects for vector-based inputs. This interface is similar
#' to [`fortify.Spat`] and is provided in case the [ggplot2::fortify()] method
#' is deprecated in the future.
#'
#' @param x A `SpatRaster` created with [terra::rast()], a `SpatVector`
#'   created with [terra::vect()], a `SpatGraticule` (see [terra::graticule()])
#'   or a `SpatExtent` (see [terra::ext()]).
#' @param ... Ignored by these methods.
#' @param pivot Logical. When `TRUE`, a `SpatRaster` is returned in [long
#'   format][tidyr::pivot_longer()]. When `FALSE` (the default), it is returned
#'   as a data frame with one column per layer. See **Details**.
#' @inheritParams geom_spatraster
#' @inheritParams as_tibble.Spat
#' @inheritParams generics::tidy
#'
#' @importFrom generics tidy
#' @export
#' @encoding UTF-8
#'
#' @family generics.methods
#' @family coerce
#'
#' @return
#'
#' [`tidy.SpatVector()`], [`tidy.SpatGraticule()`] and [`tidy.SpatExtent()`]
#' return a [`sf`][sf::st_sf] object.
#'
#' [`tidy.SpatRaster()`] returns a [tibble][tibble::tbl_df]. See **Methods**.
#'
#' @rdname tidy.Spat
#' @name tidy.Spat
#'
#' @seealso
#' [sf::st_as_sf()], [`as_tibble.Spat`], [as_spatraster()], [`fortify.Spat`],
#' [generics::tidy()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [generics::tidy()] method.
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
#' `attr(tidySpatRaster, "crs")`.
#'
#' You can convert the tidy object back to a `SpatRaster` with
#' [as_spatraster()].
#'
#' When `pivot = TRUE`, the `SpatRaster` is returned in long format (see
#' [tidyr::pivot_longer()]). The tidy object has the following columns:
#' * `x`, `y`: Coordinates of the cell centre in the corresponding CRS.
#' * `lyr`: Name of the `SpatRaster` layer associated with `value`.
#' * `value`: Cell value for the corresponding `lyr`.
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
#' # Get a SpatRaster
#' r <- system.file("extdata/volcano2.tif", package = "tidyterra") |>
#'   terra::rast() |>
#'   terra::project("EPSG:4326")
#'
#' r_tidy <- tidy(r)
#'
#' r_tidy
#'
#' # Convert back to a `SpatRaster`.
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
#' ex <- cyl |> terra::ext()
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
tidy.SpatRaster <- function(
  x,
  ...,
  .name_repair = c(
    "unique",
    "check_unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  ),
  maxcell = terra::ncell(x) * 1.1,
  pivot = FALSE
) {
  x <- resample_spat(x, maxcell)

  crs <- pull_crs(x)

  if (is.na(crs)) {
    crs <- ""
  }

  if (!pivot) {
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
#' @encoding UTF-8
#' @name tidy.Spat
tidy.SpatVector <- function(x, ...) {
  as_sf(x)
}

#' @export
#' @encoding UTF-8
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
#' @encoding UTF-8
#' @name tidy.Spat
#' @param crs Input that includes or represents a CRS. It can be an `sf/sfc`
#'   object, a `SpatRaster/SpatVector` object, a `crs` object from
#'   [sf::st_crs()], a character string (for example a [proj4
#'   string](https://proj.org/en/9.3/operations/projections/index.html)), or
#'   an integer representing an [EPSG](https://epsg.io/) code.
tidy.SpatExtent <- function(x, ..., crs = "") {
  as_sf(terra::vect(x, crs = pull_crs(crs)))
}

#' @export
generics::tidy
