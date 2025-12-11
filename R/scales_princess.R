#' Gradient scales from princess color schemes
#'
#' @description
#'
#' Implementation of the gradient palettes presented in
#' <https://leahsmyth.github.io/Princess-Colour-Schemes/index.html>. Three
#' scales are provided:
#' - `scale_*_princess_d()`: For discrete values.
#' - `scale_*_princess_c()`: For continuous values.
#' - `scale_*_princess_b()`: For binning continuous values.
#'
#' Additionally, a color palette `princess.colors()` is provided. See also
#' [grDevices::terrain.colors()] for details.
#'
#'
#' Additional parameters `...` would be passed on to:
#' - Discrete values: [ggplot2::discrete_scale()].
#' - Continuous values: [ggplot2::continuous_scale()].
#' - Binned continuous values: [ggplot2::binned_scale()].
#'
#' **Note that** \CRANpkg{tidyterra} just documents a selection of these
#' additional parameters, check the \CRANpkg{ggplot2} functions listed above to
#' see the full range of parameters accepted by these scales.
#'
#' @export
#'
#' @name scale_princess
#'
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritDotParams ggplot2::continuous_scale breaks:labels
#' @inheritDotParams ggplot2::binned_scale breaks:limits nice.breaks
#' @inheritParams scale_cross_blended
#' @param palette A valid palette name. The name is matched to the list of
#'   available palettes, ignoring upper vs. lower case. Values available are:
#'
#' ```{r, echo=FALSE, results="asis", message = FALSE, warning = FALSE}
#'
#' suppressPackageStartupMessages(library(dplyr))
#' princess_db |>
#'   pull(pal) |>
#'   unique() %>%
#'   paste0('`"', ., '"`', collapse = ", ") |>
#'   paste0(".") |>
#'   cat()
#'
#'
#' ```
#'
#' @seealso [terra::plot()], [ggplot2::scale_fill_viridis_c()]
#'
#' See also \CRANpkg{ggplot2} docs on additional `...` parameters.
#'
#' @return
#' The corresponding \CRANpkg{ggplot2} layer with the values applied to the
#' `fill/colour` aesthetics.
#'
#' @family gradients
#'
#' @source <https://github.com/LeahSmyth/Princess-Colour-Schemes>.
#'
#' @examples
#' \donttest{
#' filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")
#'
#' library(terra)
#' volcano2_rast <- rast(filepath)
#'
#' # Palette
#' plot(volcano2_rast, col = princess.colors(100))
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_princess_c()
#'
#' # Binned
#' ggplot() +
#'   geom_spatraster(data = volcano2_rast) +
#'   scale_fill_princess_b(breaks = seq(70, 200, 10), palette = "denmark")
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
#'
#' ggplot() +
#'   geom_spatraster(data = factor, aes(fill = cats)) +
#'   scale_fill_princess_d(na.value = "gray10", palette = "maori")
#' }
scale_fill_princess_d <- function(
  palette = "snow",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = princess_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}
#' @export
#' @rdname scale_princess
scale_colour_princess_d <- function(
  palette = "snow",
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = princess_pal(
      alpha = alpha,
      direction = direction,
      palette = palette
    ),
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

#' @export
#' @rdname scale_princess
scale_fill_princess_c <- function(
  palette = "snow",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::princess_db, palette = palette))

  ggplot2::continuous_scale(
    aesthetics = "fill",
    palette = scales::gradient_n_pal(princess_pal(
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
#' @rdname scale_princess
scale_colour_princess_c <- function(
  palette = "snow",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::princess_db, palette = palette))

  ggplot2::continuous_scale(
    aesthetics = "colour",
    palette = scales::gradient_n_pal(princess_pal(
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
#' @rdname scale_princess
scale_fill_princess_b <- function(
  palette = "snow",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::princess_db, palette = palette))

  ggplot2::binned_scale(
    aesthetics = "fill",
    palette = scales::gradient_n_pal(princess_pal(
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
#' @rdname scale_princess
scale_colour_princess_b <- function(
  palette = "snow",
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort("{.arg direction} must be {.field 1} or {.field -1}")
  }

  length_pal <- nrow(extract_pal(tidyterra::princess_db, palette = palette))

  ggplot2::binned_scale(
    aesthetics = "colour",
    palette = scales::gradient_n_pal(princess_pal(
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
#' @rdname scale_princess
#'
#' @inheritParams wiki.colors
#' @examples
#'
#' # Display all the princess palettes
#'
#' pals <- unique(princess_db$pal)
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
#'     col = princess.colors(ncols, i), main = i,
#'     ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#'   )
#' }
#' par(opar)
princess.colors <- function(n, palette = "snow", alpha = 1, rev = FALSE) {
  if ((n <- as.integer(n[1L])) > 0) {
    paltab <- extract_pal(tidyterra::princess_db, palette = palette)
    colors <- as.character(paltab$hex)
    endcols <- tidyterra_ramp(colors, n, alpha, rev)
    endcols
  } else {
    character()
  }
}


princess_pal <- function(alpha = 1, direction = 1, palette) {
  # nocov start
  function(n) {
    pal <- princess.colors(
      n,
      rev = direction != 1,
      alpha = alpha,
      palette = palette
    )

    pal
  }
  # nocov end
}


#' @export
#' @rdname scale_princess
#' @usage NULL
scale_color_princess_d <- scale_colour_princess_d


#' @export
#' @rdname scale_princess
#' @usage NULL
scale_color_princess_c <- scale_colour_princess_c


#' @export
#' @rdname scale_princess
#' @usage NULL
scale_color_princess_b <- scale_colour_princess_b
