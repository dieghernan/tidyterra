#' Create a complete ggplot for `Spat*` objects
#'
#' `autoplot()` uses \CRANpkg{ggplot2} to draw plots as the ones produced by
#' [terra::plot()]/[terra::plotRGB()] in a single command.
#'
#' Implementation of [ggplot2::autoplot()] method.
#'
#' @return A \CRANpkg{ggplot2} layer
#' @family ggplot2.utils
#' @family ggplot2.methods
#'
#' @param object A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()]. Also support `SpatGraticule` (see
#'   [terra::graticule()]) and `SpatExtent` (see [terra::ext()]).
#'
#' @rdname autoplot.Spat
#' @name autoplot.Spat
#'
#' @param rgb Logical. Should be plotted as a RGB image? If `NULL` (the default)
#'   [autoplot.SpatRaster()] would try to guess.
#' @param use_coltab Logical. Should be plotted with the corresponding
#'   [terra::coltab()]? If `NULL` (the default) [autoplot.SpatRaster()] would
#'   try to guess. See also [scale_fill_coltab()].
#' @param facets Logical. Should facets be displayed? If `NULL` (the default)
#'   [autoplot.SpatRaster()] would try to guess.
#'
#' @param nrow,ncol Number of rows and columns on the facet.
#' @param ... other arguments passed to [geom_spatraster()],
#'  [geom_spatraster_rgb()] or [geom_spatvector()].
#'
#'
#' @section Methods:
#'
#' Implementation of the **generic** [ggplot2::autoplot()] function.
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
#' @export
#' @importFrom ggplot2 autoplot
#'
#' @seealso [ggplot2::autoplot()]
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
#'
#' # With a tile
#'
#' tile <- system.file("extdata/cyl_tile.tif", package = "tidyterra") |>
#'   rast()
#'
#' autoplot(tile)
#'
#' # With coltabs
#'
#' ctab <- system.file("extdata/cyl_era.tif", package = "tidyterra") |>
#'   rast()
#'
#' autoplot(ctab)
#'
#' #  With vectors
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
    gg <- gg +
      geom_spatraster_rgb(
        data = object,
        ...
      )
    # Done, no facets or scales on RGB
    return(gg)
  }

  # Guess scale
  if (is.null(use_coltab)) {
    use_coltab <- any(terra::has.colors(object))
  }
  gg <- gg +
    geom_spatraster(
      data = object,
      use_coltab = use_coltab,
      ...
    )

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

  if (
    all(
      facets,
      isFALSE(rgb)
    )
  ) {
    gg <- gg + ggplot2::facet_wrap(~lyr, nrow = nrow, ncol = ncol)
  }

  gg
}

#' @export
#' @name autoplot.Spat
autoplot.SpatVector <- function(object, ...) {
  ggplot2::ggplot(data = object) +
    geom_spatvector(...)
}

#' @export
#' @name autoplot.Spat
autoplot.SpatGraticule <- function(object, ...) {
  ggplot2::ggplot(data = object) +
    geom_spatvector(...)
}

#' @export
#' @name autoplot.Spat
autoplot.SpatExtent <- function(object, ...) {
  ggplot2::ggplot(data = object) +
    geom_spatvector(...)
}

#' @export
ggplot2::autoplot
