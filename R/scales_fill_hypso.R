#' Gradient fill scales for representing hypsometry and bathymetry
#'
#' @description
#'
#' Implementation of a selection of gradient palettes available in
#' [cpt-city](http://soliton.vm.bytemark.co.uk/pub/cpt-city/). These scales can
#' act as
#' [hypsometric (or bathymetric) tints](https://en.wikipedia.org/wiki/Hypsometric_tints)
#' using the option `as_tint = TRUE`. Three fill scales are provided:
#' - `scale_fill_hypso_d()`: For discrete values.
#' - `scale_fill_hypso_c()`: For continuous values.
#' - `scale_fill_hypso_b()`: For binning continuous values.
#'
#' Additionally, a color palette `hypso.colors()` is provided. See also
#' [grDevices::terrain.colors()] for details.
#'
#' @export
#'
#' @name scale_fill_hypso
#'
#' @inheritParams ggplot2::scale_fill_viridis_b
#' @inheritParams ggplot2::continuous_scale
#' @param palette A valid palette name. The name is matched to the list of
#'   available palettes, ignoring upper vs. lower case. See
#'   [hypsometric_tints_db] for more info. Values available are:
#'
#' ```{r, echo=FALSE, results="asis"}
#'
#' library(dplyr)
#' hypsometric_tints_db %>%
#'   pull(pal) %>%
#'   unique() %>%
#'   paste0('`"', ., '"`', collapse = ", ") %>%
#'   paste0(".") %>%
#'   cat()
#'
#'
#' ```
#' @param as_tint Logical. Should the scale use predefined limits? See
#'   **Details**.
#' @seealso [hypsometric_tints_db], [terra::plot()],
#' [ggplot2::scale_fill_viridis_c()]
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill` aesthetics.
#'
#' @family gradients
#'
#' @details
#'
#' When using the parameter `as_tint = TRUE`, the position of the gradients and
#' the limits of the palette are redefined. Instead of treating the color
#' palette as a continuous gradient, they are rescaled to act as a hypsometric
#' tint. A rough description of these tints are:
#' - Blue colors: Negative values.
#' - Green colors: 0 to 1.000 values.
#' - Browns: 1000 to 4.000 values.
#' - Whites: Values higher than 4.000.
#'
#' The following orientation would vary depending on the palette definition
#' (see [hypsometric_tints_db] for an example on how this could be achieved).
#'
#' Note that the combination of the palette and `as_tint = TRUE` may not be
#' always suitable for your specific data. For example, raster of small parts
#' of the globe (and with a limited range of elevations) may not be well
#' represented. As an example, a raster with a range of values on `[100, 200]`
#' would appear almost as an uniform color.
#'
#' This could be adjusted using the `limits`/`values` provided by **ggplot2**.
#' See **Examples**.
#'
#' @source
#'
#' cpt-city: <http://soliton.vm.bytemark.co.uk/pub/cpt-city/>.
#'
#' @examples
#' \donttest{
#'
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' # Palette
#' plot(volcano2_rast, col = hypso.colors(100))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_c()
#'
#' # Use hypsometric limits...
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_c(as_tint = TRUE, palette = "wiki-schwarzwald-cont")
#'
#' # ...but not suitable for the range of the raster: adjust
#' my_lims <- c(70, 200)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_c(
#'     as_tint = TRUE, palette = "wiki-schwarzwald-cont",
#'     limits = my_lims
#'   )
#'
#' # Full map with true tints
#'
#' f_asia <- system.file("extdata/asia.tif", package = "tidyterra")
#' asia <- rast(f_asia)
#'
#' ggplot() +
#'   geom_spatraster(data = asia) +
#'   scale_fill_hypso_c(as_tint = TRUE, palette = "wiki-2.0") +
#'   labs(fill = "elevation (m)")
#'
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_b(breaks = seq(70, 200, 25), palette = "wiki-2.0_hypso")
#'
#' # With limits and breaks
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_hypso_b(
#'     breaks = seq(75, 200, 25), palette = "wiki-2.0_hypso",
#'     as_tint = TRUE,
#'     limits = c(75, 200)
#'   )
#'
#' # With discrete values
#' factor <- volcano2_rast %>% mutate(cats = cut(elevation,
#'   breaks = c(100, 120, 130, 150, 170, 200),
#'   labels = c(
#'     "Very Low", "Low", "Average", "High",
#'     "Very High"
#'   )
#' ))
#'
#'
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_hypso_d(na.value = "gray10", palette = "dem_poster")
#' }
scale_fill_hypso_d <- function(palette = "etopo1_hypso", ...,
                               alpha = 1, direction = 1) {
  if (alpha < 0 | alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "hypso_fill_d",
    palette = hypso_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    ...
  )
}
#' @export
#' @rdname scale_fill_hypso
scale_fill_hypso_c <- function(palette = "etopo1_hypso", ...,
                               alpha = 1, direction = 1,
                               values = NULL,
                               limits = NULL,
                               as_tint = FALSE,
                               na.value = NA,
                               guide = "colourbar") {
  if (alpha < 0 | alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  if (isFALSE(as_tint)) {
    ggplot2::continuous_scale(
      aesthetics = "fill",
      scale_name = "hypso_fill_c",
      scales::gradient_n_pal(hypso_pal(
        alpha = alpha,
        direction = direction,
        palette = palette
      )(100)),
      na.value = na.value,
      guide = guide,
      ...
    )
  } else {

    # Use pal limits
    coltab <- tidyterra::hypsometric_tints_db

    if (!palette %in% coltab$pal) {
      stop("'palette' does not match any given palette")
    }

    hypsocol <- coltab[coltab$pal == palette, ]
    hexcol <- as.character(hypsocol$hex)
    if (direction == -1) hexcol <- rev(hexcol)
    if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

    if (is.null(values)) values <- hypsocol$limit
    # Reescale
    res <- scales::rescale(values)
    if (is.null(limits)) limits <- range(values)

    ggplot2::scale_fill_gradientn(...,
      colors = hexcol,
      values = res,
      limits = limits,
      na.value = na.value,
      guide = guide
    )
  }
}

#' @export
#' @rdname scale_fill_hypso
scale_fill_hypso_b <- function(palette = "etopo1_hypso", ...,
                               alpha = 1, direction = 1,
                               values = NULL,
                               limits = NULL,
                               as_tint = FALSE,
                               na.value = NA, guide = "coloursteps") {
  if (alpha < 0 | alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")
  if (isFALSE(as_tint)) {
    ggplot2::binned_scale(
      aesthetics = "fill",
      scale_name = "hypso_fill_b",
      scales::gradient_n_pal(hypso_pal(
        alpha = alpha,
        direction = direction,
        palette = palette
      )(100)),
      na.value = na.value,
      guide = guide,
      ...
    )
  } else {

    # Use pal limits
    coltab <- tidyterra::hypsometric_tints_db

    if (!palette %in% coltab$pal) {
      stop("'palette' does not match any given palette")
    }

    hypsocol <- coltab[coltab$pal == palette, ]
    hexcol <- as.character(hypsocol$hex)
    if (direction == -1) hexcol <- rev(hexcol)
    if (alpha != 1) hexcol <- ggplot2::alpha(hexcol, alpha = alpha)

    if (is.null(values)) values <- hypsocol$limit
    # Reescale
    res <- scales::rescale(values)
    if (is.null(limits)) limits <- range(values)

    ggplot2::scale_fill_stepsn(
      ...,
      colors = hexcol,
      values = res,
      limits = limits,
      na.value = na.value,
      guide = guide
    )
  }
}
#' @export
#' @rdname scale_fill_hypso
#'
#' @inheritParams wiki.colors
#' @examples
#' # Display all the cpl_city palettes
#'
#' pals <- unique(hypsometric_tints_db$pal)
#'
#' # Helper fun for plotting
#'
#' ncols <- 128
#' rowcol <- grDevices::n2mfrow(length(pals))
#'
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = rowcol, mar = rep(1, 4))
#'
#' for (i in pals) {
#'   image(
#'     x = seq(1, ncols), y = 1, z = as.matrix(seq(1, ncols)),
#'     col = hypso.colors(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
hypso.colors <- function(n, palette = "etopo1_hypso",
                         alpha = 1, rev = FALSE) {
  palette <- tolower(palette)

  coltab <- tidyterra::hypsometric_tints_db

  if (!palette %in% coltab$pal) {
    stop("'palette' does not match any given palette")
  }

  if ((n <- as.integer(n[1L])) > 0) {
    hypsocol <- coltab[coltab$pal == palette, ]
    hypsocol <- as.character(hypsocol$hex)


    if (rev) hypsocol <- rev(hypsocol)
    fn_cols <- grDevices::colorRamp(hypsocol,
      space = "Lab",
      interpolate = "spline"
    )
    cols <- fn_cols(seq(0, 1, length.out = n)) / 255
    if (alpha != 1) {
      endcols <- grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
    } else {
      endcols <- grDevices::rgb(cols[, 1], cols[, 2], cols[, 3])
    }
    return(endcols)
  } else {
    character()
  }
}


hypso_pal <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- hypso.colors(n,
      rev = direction != 1, alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}
