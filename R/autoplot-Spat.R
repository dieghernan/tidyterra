#' Create a complete ggplot for `Spat*` objects
#'
#' `autoplot()` uses \CRANpkg{ggplot2} to draw plots like those
#' produced by [terra::plot()]/[terra::plotRGB()] in a single command.
#'
#' Implementation of [ggplot2::autoplot()] method.
#'
#' @export
#' @encoding UTF-8
#' @rdname autoplot.Spat
#' @name autoplot.Spat
#'
#' @seealso [ggplot2::autoplot()]
#'
#' @family ggplot2.utils
#' @family ggplot2.methods
#'
#' @importFrom ggplot2 autoplot
#'
#' @param object A `SpatRaster` created with [terra::rast()], a `SpatVector`
#'   created with [terra::vect()], a `SpatGraticule` (see [terra::graticule()])
#'   or a `SpatExtent` (see [terra::ext()]).
#'
#' @param rgb Logical. If `TRUE`, plot as an RGB image. If `NULL` (the default),
#'   [autoplot.SpatRaster()] tries to guess.
#' @param use_coltab Logical. If `TRUE`, plot with the corresponding
#'   color table from [terra::coltab()]. If `NULL` (the default),
#'   [autoplot.SpatRaster()] tries to guess. See also [scale_fill_coltab()].
#' @param facets Logical. If `TRUE`, display facets. If `NULL` (the default),
#'   [autoplot.SpatRaster()] tries to guess.
#'
#' @param nrow,ncol Number of rows and columns in the facet.
#' @param ... Other arguments passed to [geom_spatraster()],
#'   [geom_spatraster_rgb()] or [geom_spatvector()].
#'
#' @returns A \CRANpkg{ggplot2} layer.
#' @section Methods:
#'
#' Implementation of the **generic** [ggplot2::autoplot()] method.
#'
#' ## `SpatRaster`
#'
#' Uses [geom_spatraster()] or [geom_spatraster_rgb()].
#'
#' ## `SpatVector`, `SpatGraticule` and `SpatExtent`
#'
#' Uses [geom_spatvector()]. Labels can be placed with [geom_spatvector_text()]
#' or [geom_spatvector_label()].
#'
#' @examples
#' \donttest{
#'
#' file_path <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#'
#' library(terra)
#' temp <- rast(file_path)
#'
#' library(ggplot2)
#' autoplot(temp)
#'
#' # With a tile
#'
#' tile <- system.file("extdata/cyl_tile.tif", package = "tidyterra") |>
#'   rast()
#'
#' autoplot(tile)
#'
#' # With color tables
#'
#' ctab <- system.file("extdata/cyl_era.tif", package = "tidyterra") |>
#'   rast()
#'
#' autoplot(ctab)
#'
#' # With vectors
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' autoplot(v)
#'
#' v |> autoplot(aes(fill = cpro)) +
#'   geom_spatvector_text(aes(label = iso2)) +
#'   coord_sf(crs = 25829)
#' }
autoplot.SpatRaster <- function(
  object,
  ...,
  rgb = NULL,
  use_coltab = NULL,
  facets = NULL,
  nrow = NULL,
  ncol = 2
) {
  gg <- ggplot2::ggplot()

  if (is.null(rgb)) {
    rgb <- terra::has.RGB(object)
  }

  if (rgb) {
    gg <- gg + geom_spatraster_rgb(data = object, ...)
    # Done, no facets or scales on RGB
    return(gg)
  }

  # Guess scale
  if (is.null(use_coltab)) {
    use_coltab <- any(terra::has.colors(object))
  }
  gg <- gg + geom_spatraster(data = object, use_coltab = use_coltab, ...)

  if (!use_coltab) {
    todf <- terra::as.data.frame(object[1, ], na.rm = TRUE, xy = FALSE)
    first_lay <- unlist(lapply(todf, class))[1]

    if (first_lay %in% c("character", "factor")) {
      gg <- gg + scale_fill_grass_d()
    } else {
      gg <- gg + scale_fill_grass_c()
    }
  }

  # Guess facets
  if (is.null(facets)) {
    facets <- terra::nlyr(object) > 1
  }

  if (all(facets, isFALSE(rgb))) {
    gg <- gg + ggplot2::facet_wrap(~lyr, nrow = nrow, ncol = ncol)
  }

  gg
}

#' @export
#' @encoding UTF-8
#' @name autoplot.Spat
autoplot.SpatVector <- function(object, ...) {
  ggplot2::ggplot(data = object) +
    geom_spatvector(...)
}

#' @export
#' @encoding UTF-8
#' @name autoplot.Spat
autoplot.SpatGraticule <- function(object, ...) {
  ggplot2::ggplot(data = object) +
    geom_spatvector(...)
}

#' @export
#' @encoding UTF-8
#' @name autoplot.Spat
autoplot.SpatExtent <- function(object, ...) {
  ggplot2::ggplot(data = object) +
    geom_spatvector(...)
}

#' @export
ggplot2::autoplot
