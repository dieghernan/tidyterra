#' Gradient fill scales from Wikipedia color schemes
#'
#' @description
#'
#' Implementation based on the
#' [Wikipedia Colorimetric conventions for topographic maps](https://en.wikipedia.org/wiki/Wikipedia:WikiProject_Maps/Conventions/Topographic_maps).
#' Three fill scales are provided:
#' - `scale_fill_wiki_d()`: For discrete values.
#' - `scale_fill_wiki_c()`: For continuous values.
#' - `scale_fill_wiki_b()`: For binning continuous values.
#'
#' Additionally, a color palette `wiki.colors()` is provided. See also
#' [grDevices::terrain.colors()] for details.
#'
#' @export
#'
#' @name scale_fill_wiki
#'
#' @inheritParams ggplot2::scale_fill_viridis_b
#'
#' @seealso [terra::plot()], [ggplot2::scale_fill_viridis_c()]
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill` aesthetics.
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
scale_fill_wiki_d <- function(..., alpha = 1, direction = 1) {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "wiki_fill_d",
    palette = wiki_pal(
      alpha = alpha,
      direction = direction
    ),
    ...
  )
}
#' @export
#' @rdname scale_fill_wiki
scale_fill_wiki_c <- function(..., alpha = 1, direction = 1,
                              na.value = NA, guide = "colourbar") {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

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
#' @rdname scale_fill_wiki
scale_fill_wiki_b <- function(..., alpha = 1, direction = 1,
                              na.value = NA, guide = "coloursteps") {
  if (alpha < 0 || alpha > 1) {
    stop("alpha level ", alpha, " not in [0,1]")
  }

  if (!direction %in% c(-1, 1)) stop("direction must be 1 or -1")

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
#' @rdname scale_fill_wiki
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
