#' Fortify `Spat*` Objects
#'
#' Fortify `SpatRaster` and `SpatVector` objects to data frames. This provide
#' native compatibility with [ggplot2::ggplot()].
#'
#'
#' @param model A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()].
#' @param data Not used by this method.
#' @inheritParams geom_spatraster
#' @inheritParams ggplot2::fortify
#' @inheritParams as_tibble.Spat
#' @param pivot Logical. When `TRUE` the `SpatRaster` would be fortified on
#'   [long format][tidyr::pivot_longer()]. When `FALSE` (the default) it would
#'   be fortified as a data frame with a column for each layer. See **Details**.
#' @importFrom ggplot2 fortify
#' @export
#'
#' @family ggplot2.utils
#' @family ggplot2.methods
#' @family coerce
#'
#' @return
#'
#' [fortify.SpatVector()] returns a [`sf`][sf::st_sf] object and
#' [fortify.SpatRaster()] returns a [`tibble`][tibble::tibble]. See **Methods**.
#'
#' @rdname fortify.Spat
#' @name fortify.Spat
#'
#' @seealso [sf::st_as_sf()], [`as_tibble.Spat`], [as_spatraster()],
#'   [ggplot2::fortify()].
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
#' ## `SpatVector` and `SpatGraticule`
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
#' # You can now use a SpatRaster with any geom
#' library(ggplot2)
#'
#' ggplot(r) +
#'   geom_histogram(aes(x = elevation),
#'     bins = 20, fill = "lightblue",
#'     color = "black"
#'   )
#'
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
#' # Now you can use geom_sf() straight away thanks to fortify::SpatVector()
#'
#' library(ggplot2)
#'
#' ggplot(cyl) +
#'   geom_sf()
#' }
#'
fortify.SpatRaster <- function(model, data, ..., .name_repair = "unique",
                               maxcell = terra::ncell(model) * 1.1,
                               pivot = FALSE) {
  model <- resample_spat(model, maxcell)

  crs <- pull_crs(model)

  if (is.na(crs)) crs <- ""

  if (pivot == FALSE) {
    model <- as_tibble(model, xy = TRUE, .name_repair = .name_repair)
  } else {
    model <- check_mixed_cols(model,
      fn = "tidyterra::fortify.SpatRaster"
    )
    model <- pivot_longer_spat(model)
    attr(model, "pvt_fort") <- TRUE
  }


  attr(model, "crs") <- crs

  model
}

#' @export
#' @name fortify.Spat
fortify.SpatVector <- function(model, data, ...) {
  as_sf(model)
}

#' @export
#' @name fortify.Spat
fortify.SpatGraticule <- function(model, data, ...) {
  # nocov start
  tvers <- packageVersion("terra")
  if (tvers < "1.8.5") {
    msg <- paste(
      "Need {.pkg terra} {.strong 1.8.5} or later for ",
      "{.fn fortify.SpatGraticule} method. Current {.pkg terra} ",
      "version is  {.strong {tvers}}"
    )
    cli::cli_abort(msg)
  }
  # nocov end
  as_sf(terra::vect(model))
}

#' @export
ggplot2::fortify
