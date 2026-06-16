#' Gradient scales from **WhiteboxTools** color schemes
#'
#' @description
#'
#' Implementation of the gradient palettes provided by
#' [WhiteboxTools](https://github.com/jblindsay/whitebox-tools). Three
#' scales are provided:
#' - `scale_*_whitebox_d()`: For discrete values.
#' - `scale_*_whitebox_c()`: For continuous values.
#' - `scale_*_whitebox_b()`: For binning continuous values.
#'
#' Additionally, a color palette `whitebox.colors()` is provided. See also
#' [grDevices::terrain.colors()] for details.
#'
#' Additional arguments `...` are passed to:
#' - Discrete values: [ggplot2::discrete_scale()].
#' - Continuous values: [ggplot2::continuous_scale()].
#' - Binned continuous values: [ggplot2::binned_scale()].
#'
#' \CRANpkg{tidyterra} documents only a selection of these additional
#' arguments, so check the \CRANpkg{ggplot2} functions listed above to see the
#' full range of arguments accepted by these scales.
#'
#' @source <https://github.com/jblindsay/whitebox-tools>, under
#' MIT License. Copyright (c) 2017-2021 John Lindsay.
#'
#' @export
#' @encoding UTF-8
#'
#' @name scale_whitebox
#'
#' @seealso [terra::plot()], [ggplot2::scale_fill_viridis_c()]
#'
#' See also \CRANpkg{ggplot2} docs on additional `...` arguments.
#'
#' @family gradients
#'
#' @inheritParams scale_cross_blended
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
#' @param palette A valid palette name. The name is matched to the list of
#'   available palettes, ignoring upper vs. lower case. The available values
#'   are listed below.
#'
#' ```{r, echo=FALSE, results="asis", message = FALSE, warning = FALSE}
#'
#' suppressPackageStartupMessages(library(dplyr))
#' whitebox_coltab |>
#'   pull(pal) |>
#'   unique() %>%
#'   paste0('`"', ., '"`', collapse = ", ") |>
#'   paste0(".") |>
#'   cat()
#'
#' ```
#'
#' @returns
#' The corresponding \CRANpkg{ggplot2} layer with the values applied to the
#' `fill/colour` aesthetics.
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
#' factor <- volcano2_rast |> mutate(cats = cut(elevation,
#'   breaks = c(100, 120, 130, 150, 170, 200),
#'   labels = c(
#'     "Very Low", "Low", "Average", "High",
#'     "Very High"
#'   )
#' ))
#'
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_whitebox_d(na.value = "gray10", palette = "soft")
#' }
scale_fill_whitebox_d <- function(
  palette = "high_relief",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "fill",
    whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}
#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
scale_colour_whitebox_d <- function(
  palette = "high_relief",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  pal_discrete_scale(
    "colour",
    whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    alpha = alpha,
    direction = direction,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
scale_fill_whitebox_c <- function(
  palette = "high_relief",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  pal_gradient_scale(
    ggplot2::continuous_scale,
    "fill",
    whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() nrow(extract_pal(whitebox_coltab, palette = palette)),
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
scale_colour_whitebox_c <- function(
  palette = "high_relief",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  pal_gradient_scale(
    ggplot2::continuous_scale,
    "colour",
    whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() nrow(extract_pal(whitebox_coltab, palette = palette)),
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
scale_fill_whitebox_b <- function(
  palette = "high_relief",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  pal_gradient_scale(
    ggplot2::binned_scale,
    "fill",
    whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() nrow(extract_pal(whitebox_coltab, palette = palette)),
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
scale_colour_whitebox_b <- function(
  palette = "high_relief",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  pal_gradient_scale(
    ggplot2::binned_scale,
    "colour",
    whitebox_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    n = function() nrow(extract_pal(whitebox_coltab, palette = palette)),
    alpha = alpha,
    direction = direction,
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
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
#' # Helper function for plotting
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
whitebox.colors <- function(
  n,
  palette = "high_relief",
  alpha = 1,
  rev = FALSE
) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(whitebox_coltab, palette = palette)
    colors <- as.character(paltab$hex)
    endcols <- tidyterra_ramp(colors, n, alpha, rev)
    endcols
  } else {
    character()
  }
}

whitebox_pal <- function(alpha = 1, direction = 1, palette) {
  function(n) {
    pal <- whitebox.colors(
      n,
      rev = direction != 1,
      alpha = alpha,
      palette = palette
    )

    pal
  }
}

extract_pal <- function(df, palette) {
  palette <- tolower(palette)

  check_palette(palette, df$pal)

  df <- df[df$pal == palette, ]
  df
}

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
#' @usage NULL
scale_color_whitebox_d <- scale_colour_whitebox_d

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
#' @usage NULL
scale_color_whitebox_c <- scale_colour_whitebox_c

#' @export
#' @encoding UTF-8
#' @rdname scale_whitebox
#' @usage NULL
scale_color_whitebox_b <- scale_colour_whitebox_b
