#' Gradient scales from Wikipedia color schemes
#'
#' @description
#'
#' Implementation based on the
#' [Wikipedia Colorimetric conventions for topographic
#' maps](https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps).
#' Three scales are provided:
#' - `scale_*_wiki_d()`: For discrete values.
#' - `scale_*_wiki_c()`: For continuous values.
#' - `scale_*_wiki_b()`: For binning continuous values.
#'
#' Additionally, a color palette `wiki.colors()` is provided. See also
#' [grDevices::terrain.colors()] for details.
#'
#' Additional parameters `...` would be passed on to:
#' - Discrete values: [ggplot2::discrete_scale()]
#' - Continuous values: [ggplot2::continuous_scale()]
#' - Binned continuous values: [ggplot2::binned_scale()].
#'
#' Note that \pkg{tidyterra} just documents a selection of these additional
#' parameters, check the previous links to see the full range of parameters
#' accepted by these scales.
#'
#'
#' @export
#'
#' @name scale_wiki
#'
#' @inheritParams scale_cross_blended
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
#' @inheritParams ggplot2::scale_fill_viridis_b
#' @inheritParams ggplot2::continuous_scale
#' @seealso [terra::plot()], [ggplot2::scale_fill_viridis_c()]
#'
#' See also \pkg{ggplot2} docs on additional `...` parameters:
#'
#' - `scale_*_terrain_d()`: For discrete values.
#' - `scale_*_terrain_c()`: For continuous values.
#' - `scale_*_terrain_b()`: For binning continuous values.
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill/colour` aesthetics.
#'
#' @family gradients
#'
#' @importFrom ggplot2 alpha
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' # Palette
#' plot(volcano2_rast, col = wiki.colors(100))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_wiki_c()
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_wiki_b(breaks = seq(70, 200, 10))
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
#'   scale_fill_wiki_d(na.value = "gray10")
#' }
scale_fill_wiki_d <- function(..., alpha = 1, direction = 1,
                              na.translate = FALSE,
                              drop = TRUE) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "wiki_fill_d",
    palette = wiki_pal(
      alpha = alpha,
      direction = direction
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @rdname scale_wiki
scale_colour_wiki_d <- function(..., alpha = 1, direction = 1,
                                na.translate = FALSE,
                                drop = TRUE) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "wiki_colour_d",
    palette = wiki_pal(
      alpha = alpha,
      direction = direction
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @rdname scale_wiki
#' @usage NULL
scale_color_wiki_d <- scale_colour_wiki_d

#' @export
#' @rdname scale_wiki
scale_fill_wiki_c <- function(..., alpha = 1, direction = 1,
                              na.value = "transparent", guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- length(wiki_cols)

  ggplot2::continuous_scale(
    aesthetics = "fill",
    scale_name = "wiki_fill_c",
    scales::gradient_n_pal(wiki_pal(
      alpha = alpha,
      direction = direction
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_wiki
scale_colour_wiki_c <- function(..., alpha = 1, direction = 1,
                                na.value = "transparent", guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- length(wiki_cols)

  ggplot2::continuous_scale(
    aesthetics = "colour",
    scale_name = "wiki_colour_c",
    scales::gradient_n_pal(wiki_pal(
      alpha = alpha,
      direction = direction
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_wiki
#' @usage NULL
scale_color_wiki_c <- scale_colour_wiki_c

#' @export
#' @rdname scale_wiki
scale_fill_wiki_b <- function(..., alpha = 1, direction = 1,
                              na.value = "transparent", guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- length(wiki_cols)

  ggplot2::binned_scale(
    aesthetics = "fill",
    scale_name = "wiki_fill_b",
    scales::gradient_n_pal(wiki_pal(
      alpha = alpha,
      direction = direction
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_wiki
scale_colour_wiki_b <- function(..., alpha = 1, direction = 1,
                                na.value = "transparent",
                                guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- length(wiki_cols)

  ggplot2::binned_scale(
    aesthetics = "colour",
    scale_name = "wiki_colour_b",
    scales::gradient_n_pal(wiki_pal(
      alpha = alpha,
      direction = direction
    )(length_pal)),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_wiki
#' @usage NULL
scale_color_wiki_b <- scale_colour_wiki_b

#' @export
#' @rdname scale_wiki
#' @inheritParams grDevices::terrain.colors
wiki.colors <- function(n, alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    colors <- wiki_cols
    endcols <- tidyterra_ramp(colors, n, alpha, rev)
    return(endcols)
  } else {
    character()
  }
}

# Create ramp
# Create ramp
tidyterra_ramp <- function(colors, n, alpha = 1, rev = FALSE) {
  if (rev) colors <- rev(colors)
  fn_cols <- scales::colour_ramp(colors, alpha = FALSE)
  endcols <- fn_cols(seq(0, 1, length.out = n))
  if (alpha != 1) endcols <- ggplot2::alpha(endcols, alpha)

  return(endcols)
}


wiki_cols <- c(
  "#3F6B48", "#5F835E", "#7F9B74", "#A0B38B", "#C0CBA1", "#E1E4B8",
  "#EFEBC0", "#E8E1B6", "#DDD6AA", "#D3CA9D", "#CAB982", "#C3A76B",
  "#B9985A", "#AA8753", "#AC9A7C", "#BAAE9A", "#CAC3B8", "#E0DED8",
  "#F5F4F2"
)

wiki_pal <- function(alpha = 1, direction = 1) {
  # nocov start
  function(n) {
    pal <- wiki.colors(n, rev = direction != 1, alpha = alpha)

    pal
  }
  # nocov end
}
