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
#' @seealso [sf::st_as_sf()], [as_tibble.Spat], [as_spatraster()],
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
#' ## `SpatVector`
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
#' # You can now use a SpatRaster with raster, contours, etc.
#' library(ggplot2)
#'
#' # Use here the SpatRaster as a regular object
#' ggplot(r, aes(x, y, z = elevation)) +
#'   stat_summary_hex(fun = mean, bins = 30, color = "white", linewidth = 0.1) +
#'   scale_fill_gradientn(
#'     colours = c(
#'       "#80146E", "#804AA4", "#6771B8", "#379BC2",
#'       "#3ABCBF", "#79D4B8", "white", "#FCEE88",
#'       "#F8C34B", "#F7A72B", "#F4792D"
#'     ),
#'     values = c(
#'       0, 0.01, 0.02, 0.1, 0.3, 0.6, 0.8, 0.85,
#'       0.9, 0.95, 1
#'     )
#'   ) +
#'   # Give spatial flavor to axis
#'   coord_fixed(
#'     xlim = c(174.761, 174.769),
#'     ylim = c(-36.88, -36.872), expand = TRUE
#'   ) +
#'   scale_x_continuous(
#'     name = "", n.breaks = 5,
#'     labels = scales::label_number(
#'       accuracy = 0.001,
#'       suffix = "°"
#'     )
#'   ) +
#'   scale_y_continuous(
#'     name = "", n.breaks = 5,
#'     labels = scales::label_number(
#'       accuracy = 0.001,
#'       suffix = "°"
#'     )
#'   )
#'
#'
#' # Or any other geom
#' ggplot(r) +
#'   geom_histogram(aes(x = elevation),
#'     bins = 20, fill = "lightblue",
#'     color = "black"
#'   )
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
  as_sf(model)
}

#' @export
ggplot2::fortify
