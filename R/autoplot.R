#' Create a complete ggplot for SpatRasters
#'
#' `autoplot()` uses ggplot2 to draw plots as the ones produced by
#' [terra::plot()]/[terra::plotRGB()] in a single command.
#'
#' Implementation of [ggplot2::autoplot()].
#'
#' @return A ggplot2 layer
#' @family ggplot2.utils
#' @param object A SpatRaster object.
#'
#' @rdname autoplot
#' @name autoplot
#'
#' @inheritParams geom_spatraster
#' @param rgb Logical. Should be plotted as a RGB image?
#' @param facets Logical. Should facets be displayed?
#' @param nrow,ncol Number of rows and columns on the facet.
#' @param ... other arguments passed to [geom_spatraster()] or
#'   [geom_spatraster_rgb()].
#'
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
#' system.file("extdata/cyl_tile.tif", package = "tidyterra") %>%
#'   rast() %>%
#'   autoplot(rgb = TRUE)
#' }
autoplot.SpatRaster <- function(object,
                                mapping = aes(),
                                rgb = FALSE,
                                facets = TRUE,
                                nrow = NULL, ncol = 2,
                                ...) {
  gg <- ggplot2::ggplot()


  if (rgb) {
    gg <- gg +
      geom_spatraster_rgb(
        data = object,
        mapping = mapping,
        ...
      )
  } else {
    gg <- gg +
      geom_spatraster(
        data = object,
        mapping = mapping,
        ...
      )

    # Guess scale
    todf <- terra::as.data.frame(object, na.rm = TRUE, xy = FALSE)
    first_lay <- unlist(lapply(todf, class))[1]

    if (first_lay %in% c("character", "factor")) {
      gg <- gg + scale_fill_terrain_d()
    } else {
      gg <- gg + scale_fill_terrain_c()
    }
  }

  if (all(
    facets,
    isFALSE(rgb)
  )) {
    gg <- gg + ggplot2::facet_wrap(~lyr, nrow = nrow, ncol = ncol)
  }

  gg
}
