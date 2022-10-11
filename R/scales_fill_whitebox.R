#' Gradient fill scales from WhiteboxTools color schemes
#'
#' @description
#'
#' Implementation of the gradient palettes provided by
#' [WhiteboxTools](https://github.com/jblindsay/whitebox-tools). Three fill
#' scales are provided:
#' - `scale_fill_whitebox_d()`: For discrete values.
#' - `scale_fill_whitebox_c()`: For continuous values.
#' - `scale_fill_whitebox_b()`: For binning continuous values.
#'
#' Additionally, a color palette `whitebox.colors()` is provided. See also
#' [grDevices::terrain.colors()] for details.
#'
#' @export
#'
#' @name scale_fill_whitebox
#'
#' @inheritParams ggplot2::scale_fill_viridis_b
#' @param palette A valid palette name. The name is matched to the list of
#'   available palettes, ignoring upper vs. lower case. Values available are:
#'
#' ```{r, echo=FALSE, results="asis", message = FALSE, warning = FALSE}
#'
#' suppressPackageStartupMessages(library(dplyr))
#' whitebox_coltab %>%
#'   pull(pal) %>%
#'   unique() %>%
#'   paste0('`"', ., '"`', collapse = ", ") %>%
#'   paste0(".") %>%
#'   cat()
#'
#'
#' ```
#'
#' @seealso [terra::plot()], [ggplot2::scale_fill_viridis_c()]
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill` aesthetics.
#'
#' @family gradients
#'
#' @source <https://github.com/jblindsay/whitebox-tools>, under
#' MIT License. Copyright (c) 2017-2021 John Lindsay.
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' # Palette
#' plot(volcano2_rast, col = whitebox.colors(100))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_whitebox_c()
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_whitebox_b(breaks = seq(70, 200, 10), palette = "atlas")
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
#'   scale_fill_whitebox_d(na.value = "gray10", palette = "soft")
#' }
scale_fill_whitebox_d <- function(palette = "high_relief", ...,
                                  alpha = 1, direction = 1) {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "whitebox_fill_d",
    palette = whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    ...
  )
}
#' @export
#' @rdname scale_fill_whitebox
scale_fill_whitebox_c <- function(palette = "high_relief", ...,
                                  alpha = 1, direction = 1,
                                  na.value = NA, guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  length_pal <- nrow(extract_pal(whitebox_coltab, palette = palette))

  ggplot2::continuous_scale(
    aesthetics = "fill",
    scale_name = "whitebox_fill_c",
    scales::gradient_n_pal(whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_fill_whitebox
scale_fill_whitebox_b <- function(palette = "high_relief", ...,
                                  alpha = 1, direction = 1,
                                  na.value = NA, guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  length_pal <- nrow(extract_pal(whitebox_coltab, palette = palette))


  ggplot2::binned_scale(
    aesthetics = "fill",
    scale_name = "whitebox_fill_b",
    scales::gradient_n_pal(whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}
#' @export
#' @rdname scale_fill_whitebox
#'
#' @inheritParams wiki.colors
#' @examples
#'
#' # Display all the whitebox palettes
#'
#' pals <- c(
#'   "atlas", "high_relief", "arid", "soft", "muted", "purple",
#'   "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
#' )
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
#'     col = whitebox.colors(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
whitebox.colors <- function(n, palette = "high_relief",
                            alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(whitebox_coltab, palette = palette)
    colors <- as.character(paltab$hex)
    endcols <- tidyterra_ramp(colors, n, alpha, rev)
    return(endcols)
  } else {
    character()
  }
}


whitebox_pal <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- whitebox.colors(n,
      rev = direction != 1, alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}

extract_pal <- function(df, palette) {
  palette <- tolower(palette)

  if (!palette %in% df$pal) {
    stop("'palette' does not match any given palette")
  }

  df <- df[df$pal == palette, ]
  return(df)
}
