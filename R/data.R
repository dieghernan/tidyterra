#' Updated Topographic Information on Auckland's Maungawhau Volcano
#'
#' @description
#'
#' Probably you already know the [volcano] dataset. This dataset provides updated
#' information of Maungawhau (Mt. Eden) from
#' [Toitu Te Whenua Land Information New Zealand](https://data.linz.govt.nz/),
#' the Government's agency that provides free online access to New Zealand’s
#' most up-to-date land and seabed data.
#'
#' @docType data
#'
#' @family datasets
#'
#' @name volcano2
#' @format
#' A matrix of `r dim(volcano2)[1]` rows and `r dim(volcano2)[2]` columns. Each
#' value is the corresponding altitude in meters.
#'
#' @source
#'
#' [Auckland LiDAR 1m DEM (2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/)
#'
#' DEM for LiDAR data from the Auckland region captured in 2013. The original
#' data has been downsampled to a resolution of 5m due to disk space constrains.
#'
#' Data License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
#'
#' @seealso [volcano]
#'
#' @note
#'  Information needed for regenerating the original raster file:
#'  - resolution: `c(5, 5)`
#'  - extent: `1756969, 1757579, 5917003, 5917873`  (xmin, xmax, ymin, ymax)
#'  - coord. ref. : NZGD2000 / New Zealand Transverse Mercator 2000
#'    (`EPSG:2193`)
#'
#' @examples
#'
#' data("volcano2")
#' filled.contour(volcano2, color.palette = hypso.colors, asp = 1)
#' title(main = "volcano2 data: filled contour map")
#'
#' # Geo-tag
#' # Empty raster
#'
#' volcano2_raster <- terra::rast(volcano2)
#' terra::crs(volcano2_raster) <- pull_crs(2193)
#' terra::ext(volcano2_raster) <- c(1756968, 1757576, 5917000, 5917872)
#' names(volcano2_raster) <- "volcano2"
#'
#' library(ggplot2)
#'
#' ggplot() +
#'   geom_spatraster(data = volcano2_raster) +
#'   scale_fill_hypso_c() +
#'   labs(
#'     title = "volcano2 SpatRaster",
#'     subtitle = "Georeferenced",
#'     fill = "Elevation (m)"
#'   )
NULL

#' Hypsometric palettes database
#'
#' @description
#' A tibble including the color map of
#' `r length(unique(hypsometric_tints_db$pal))` gradient palettes. All the
#' palettes includes also a definition of colors limits in terms of elevation
#' (meters), that can be used with [ggplot2::scale_fill_gradientn()].
#'
#' @docType data
#'
#' @family datasets
#'
#' @name hypsometric_tints_db
#' @format
#' A tibble of `r nrow(hypsometric_tints_db)` rows and
#' `r ncol(hypsometric_tints_db)` columns. with the following fields:
#' - **pal**: Name of the palette.
#' - **limit**: Recommended elevation limit (in meters) for each color.
#' - **r**,**g**,**b**: Value of the red, green and blue channel (RGB color
#'   mode).
#' - **hex**: Hex code of the color.
#'
#' @source
#' cpt-city: <http://soliton.vm.bytemark.co.uk/pub/cpt-city/>.
#'
#' @seealso [scale_fill_hypso_c()]
#'
#' @examples
#' \donttest{
#' data("hypsometric_tints_db")
#'
#' hypsometric_tints_db
#'
#' # Select a palette
#' wikicols <- hypsometric_tints_db %>%
#'   filter(pal == "wiki-2.0")
#'
#' f <- system.file("extdata/asia.tif", package = "tidyterra")
#' r <- terra::rast(f)
#'
#' library(ggplot2)
#'
#' p <- ggplot() +
#'   geom_spatraster(data = r) +
#'   labs(fill = "elevation")
#'
#' p +
#'   scale_fill_gradientn(colors = wikicols$hex)
#'
#' # Use with limits
#' p +
#'   scale_fill_gradientn(
#'     colors = wikicols$hex,
#'     values = scales::rescale(wikicols$limit),
#'     limit = range(wikicols$limit)
#'   )
#' }
NULL
