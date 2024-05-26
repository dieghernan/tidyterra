#' Updated topographic information on Auckland's Maungawhau volcano
#'
#' @description
#'
#' Probably you already know the [volcano] dataset. This dataset provides
#' updated information of Maungawhau (Mt. Eden) from
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
#' [Auckland LiDAR 1m DEM
#' (2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/).
#'
#' DEM for LiDAR data from the Auckland region captured in 2013. The original
#' data has been downsampled to a resolution of 5m due to disk space constrains.
#'
#' Data License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
#'
#' @seealso [volcano]
#'
#' @note
#'  Information needed for regenerating the original `SpatRaster` file:
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
#' A [`tibble`][tibble::tibble()] including the color map of
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
#' A [`tibble`][tibble::tibble()] of `r nrow(hypsometric_tints_db)` rows and
#' `r ncol(hypsometric_tints_db)` columns. with the following fields:
#'
#' \describe{
#'   \item{pal}{ Name of the palette.}
#'   \item{limit}{Recommended elevation limit (in meters) for each color.}
#'   \item{r}{Value of the red channel (RGB color mode).}
#'   \item{g}{Value of the green channel (RGB color mode).}
#'   \item{b}{Value of the blue channel (RGB color mode).}
#'   \item{hex}{ Hex code of the color.}
#' }
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

#' Cross-blended hypsometric tints
#'
#' @description
#' A [`tibble`][tibble::tibble()] including the color map of
#' `r length(unique(cross_blended_hypsometric_tints_db$pal))` gradient palettes.
#' All the palettes includes also a definition of colors limits in terms of
#' elevation (meters), that can be used with [ggplot2::scale_fill_gradientn()].
#'
#' @docType data
#'
#' @family datasets
#'
#' @name cross_blended_hypsometric_tints_db
#' @format
#' A tibble of `r nrow(cross_blended_hypsometric_tints_db)` rows and
#' `r ncol(cross_blended_hypsometric_tints_db)` columns. with the following
#' fields:
#'
#' \describe{
#'   \item{pal}{ Name of the palette.}
#'   \item{limit}{Recommended elevation limit (in meters) for each color.}
#'   \item{r}{Value of the red channel (RGB color mode).}
#'   \item{g}{Value of the green channel (RGB color mode).}
#'   \item{b}{Value of the blue channel (RGB color mode).}
#'   \item{hex}{ Hex code of the color.}
#' }
#'
#' @source
#'
#' Derived from:
#' - Patterson, T., & Jenny, B. (2011). The Development and Rationale of
#'   Cross-blended Hypsometric Tints. *Cartographic Perspectives,* (69),
#'   31 - 46. \doi{10.14714/CP69.20}.
#'
#' @details
#'  From Patterson & Jenny (2011):
#'
#'    *More recently, the role and design of hypsometric tints have come under
#'     scrutiny. One reason for this is the concern that people misread
#'     elevation colors as climate or vegetation information. Cross-blended
#'     hypsometric tints, introduced in 2009, are a partial solution to this
#'     problem. They use variable lowland colors customized to match the
#'     differing natural environments of world regions, which merge into
#'     one another.*
#'
#' @seealso [scale_fill_cross_blended_c()]
#'
#' @examples
#' \donttest{
#'
#' data("cross_blended_hypsometric_tints_db")
#'
#' cross_blended_hypsometric_tints_db
#'
#' # Select a palette
#' warm <- cross_blended_hypsometric_tints_db %>%
#'   filter(pal == "warm_humid")
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
#'   scale_fill_gradientn(colors = warm$hex)
#'
#' # Use with limits
#' p +
#'   scale_fill_gradientn(
#'     colors = warm$hex,
#'     values = scales::rescale(warm$limit),
#'     limit = range(warm$limit),
#'     na.value = "lightblue"
#'   )
#' }
NULL

#' GRASS color tables
#'
#' @description
#' A [`tibble`][tibble::tibble()] including the color map of
#' `r length(unique(grass_db$pal))` gradient palettes.
#' Some palettes includes also a definition of colors limitsthat can be used
#' with [ggplot2::scale_fill_gradientn()].
#'
#' @docType data
#'
#' @family datasets
#'
#' @name grass_db
#' @format
#' A tibble of `r nrow(grass_db)` rows and `r ncol(grass_db)` columns. with
#' the following fields:
#'
#' \describe{
#'   \item{pal}{ Name of the palette.}
#'   \item{limit}{(Optional) limit for each color.}
#'   \item{r}{Value of the red channel (RGB color mode).}
#'   \item{g}{Value of the green channel (RGB color mode).}
#'   \item{b}{Value of the blue channel (RGB color mode).}
#'   \item{hex}{ Hex code of the color.}
#' }
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::map.pal()]
#'
#' @source
#'
#' Derived from <https://github.com/OSGeo/grass/tree/main/lib/gis/colors>. See
#' also [r.color - GRASS GIS
#' Manual](https://grass.osgeo.org/grass83/manuals/r.colors.html).
#'
#' @references
#' GRASS Development Team (2024). *Geographic Resources Analysis Support System
#' (GRASS) Software, Version 8.3.2*. Open Source Geospatial Foundation, USA.
#' <https://grass.osgeo.org>.
#'
#' @details
#' Summary of palettes provided, description and recommeded use:
#'
#' ```{r child = "man/chunks/grassdec.Rmd"}
#' ```
#'
#' @seealso [scale_fill_cross_grass_c()]
#'
#' @examples
#' \donttest{
#' data("grass_db")
#'
#' grass_db
#' # Select a palette
#'
#' srtm_plus <- grass_db %>%
#'   filter(pal == "srtm_plus")
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
#'   scale_fill_gradientn(colors = srtm_plus$hex)
#'
#' # Use with limits
#' p +
#'   scale_fill_gradientn(
#'     colors = srtm_plus$hex,
#'     values = scales::rescale(srtm_plus$limit),
#'     limit = range(srtm_plus$limit),
#'     na.value = "lightblue"
#'   )
#' }
NULL
