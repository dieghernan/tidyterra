#' Hypsometric tints fill scales
#'
#' @description
#'
#' Implementation of the
#' [Wikipedia Colorimetric conventions for topographic maps](https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps).
#' Two fill scales are provided:
#' - `scale_fill_topo_c()`: For continuous values.
#' - `scale_fill_topo_b()`: For binning continuous values.
#'
#'
#' @export
#'
#' @name scale_fill_topo
#'
#' @inheritParams ggplot2::scale_fill_viridis_b
#'
#' @seealso [terra::plot()], [ggplot2::scale_fill_viridis_c()]
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill` aesthetics.
#'
#' @family ggplot2.utils
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
#'
#' library(terra)
#' cyl_elev <- rast(filepath)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = cyl_elev) +
#'   scale_fill_topo_c()
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = cyl_elev) +
#'   scale_fill_topo_b(breaks = c(100, 250, 500, 1000, 1500, 2000, 2500))
#' }
scale_fill_topo_c <- function(..., alpha = 1, direction = 1,
                              na.value = NA, guide = "colourbar") {
  if (alpha < 0 | alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  ggplot2::continuous_scale(
    aesthetics = "fill",
    scale_name = "topo_fill_c",
    scales::gradient_n_pal(topo_pal(
      alpha = alpha,
      direction = direction
    )(100)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_fill_topo
scale_fill_topo_b <- function(..., alpha = 1, direction = 1,
                              na.value = NA, guide = "coloursteps") {
  if (alpha < 0 | alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  ggplot2::binned_scale(
    aesthetics = "fill",
    scale_name = "topo_fill_b",
    scales::gradient_n_pal(topo_pal(
      alpha = alpha,
      direction = direction
    )(100)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

topo_pal <- function(alpha = 1, direction = 1) {
  topocol <- c(
    "#F5F4F2", "#E0DED8", "#CAC3B8", "#BAAE9A", "#AC9A7C", "#AA8753",
    "#B9985A", "#C3A76B", "#CAB982", "#D3CA9D", "#DED6A3", "#E8E1B6",
    "#EFEBC0", "#E1E4B5", "#D1D7AB", "#BDCC96", "#A8C68F", "#94BF8B",
    "#ACD0A5"
  )

  if (direction == 1) topocol <- rev(topocol)
  if (alpha != 1) {
    topocol <- adjustcolor(topocol, alpha.f = alpha)
    topocol_pal <- grDevices::colorRampPalette(topocol, alpha = TRUE)
  } else {
    topocol_pal <- grDevices::colorRampPalette(topocol, alpha = FALSE)
  }



  # nocov start
  function(n) {
    pal <- topocol_pal(n)
    pal
  }
  # nocov end
}
