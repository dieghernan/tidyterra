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
#' A matrix of 109 rows and 76 columns. Each value is the corresponding
#' altitude in meters.
#'
#' @source
#'
#' [NZ 8m Digital Elevation Model (2012)](https://data.linz.govt.nz/layer/51768-nz-8m-digital-elevation-model-2012/)
#'
#' 8m Digital Elevation Model (DEM) created by Geographx, primarily derived
#' from January 2012 LINZ Topo50 20m contours
#' (<https://data.linz.govt.nz/layer/50768-nz-contours-topo-150k/>).
#'
#' @seealso [volcano]
#'
#' @note
#'  Information needed for regenerating the original raster file:
#'  - resolution: 8
#'  - extent: 1756968, 1757576, 5917000, 5917872  (xmin, xmax, ymin, ymax)
#'  - coord. ref. : NZGD2000 / New Zealand Transverse Mercator 2000 (EPSG:2193)
#'
#' @examples
#'
#' data("volcano2")
#' filled.contour(volcano2, color.palette = terrain.colors, asp = 1)
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
#'   scale_fill_topo_c() +
#'   labs(
#'     title = "volcano2 SpatRaster",
#'     subtitle = "Georeferenced",
#'     fill = "Elevation (m)"
#'   )
NULL
