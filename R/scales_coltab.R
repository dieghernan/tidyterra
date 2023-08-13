#' Discrete scales based in the color table of a SpatRaster
#'
#' @description
#'
#' Some categorical SpatRaster may have an associated color table. This
#' function extract those values. These functions generates scales and vector
#' of colors based on the color table [terra::coltab()] associated to a
#' SpatRaster.
#'
#' You can also get a vector of colors named with the corresponding
#' factor with [get_coltab_pal()].
#'
#' Additional parameters `...` would be passed on to
#' [ggplot2::discrete_scale()]. Note that \pkg{tidyterra} just documents a
#' selection of these additional parameters, check the previous link to see the
#' full range of parameters accepted by this scale.
#' @export
#'
#' @name scale_coltab
#' @rdname scale_coltab
#'
#' @inheritDotParams ggplot2::discrete_scale breaks:drop
#' @inheritParams scale_cross_blended
#' @param data,x A SpatRaster with one or several color tables. See
#'   [terra::has.colors()].
#'
#' @seealso [terra::coltab()], [ggplot2::discrete_scale()],
#'   [ggplot2::scale_fill_manual()],
#'
#' @return The corresponding ggplot2 layer with the values applied to the
#' `fill/colour` aesthetics.
#'
#' @examples
#' library(terra)
#' # Geological Eras
#' # Spanish Geological Survey (IGME)
#'
#' r <- rast(system.file("extdata/cyl_era.tif", package = "tidyterra"))
#'
#' plot(r)
#'
#' # Get coltab
#' coltab_pal <- get_coltab_pal(r)
#'
#' coltab_pal
#'
#' \donttest{
#' # With ggplot2 + tidyterra
#' library(ggplot2)
#'
#' gg <- ggplot() +
#'   geom_spatraster(data = r)
#'
#'
#' # Default plot
#' gg
#'
#' # With coltabs
#' gg +
#'   scale_fill_coltab(data = r)
#' }
scale_fill_coltab <- function(data, ..., alpha = 1,
                              na.translate = FALSE,
                              na.value = NA,
                              drop = TRUE) {
  getcols <- get_coltab_pal(data)
  if (is.null(getcols)) {
    return(ggplot2::geom_blank())
  }

  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }
  getcols <- ggplot2::alpha(getcols, alpha = alpha)
  if (isTRUE(na.translate)) {
    # Unname
    getcols <- unname(getcols)
  }

  ggplot2::scale_fill_manual(...,
    values = getcols,
    na.translate = na.translate,
    na.value = na.value,
    drop = drop
  )
}

#' @rdname scale_coltab
#' @export
scale_colour_coltab <- function(data, ..., alpha = 1, na.translate = FALSE,
                                na.value = NA, drop = TRUE) {
  getcols <- get_coltab_pal(data)
  if (is.null(getcols)) {
    return(ggplot2::geom_blank())
  }

  if (alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg alpha} {.field {alpha}} not in {.field [0,1]}")
  }
  getcols <- ggplot2::alpha(getcols, alpha = alpha)
  if (isTRUE(na.translate)) {
    # Unname
    getcols <- unname(getcols)
  }

  ggplot2::scale_colour_manual(...,
    values = getcols,
    na.translate = na.translate,
    na.value = na.value,
    drop = drop
  )
}

#' @export
#' @rdname scale_wiki
#' @usage NULL
scale_color_coltab <- scale_colour_coltab


#' @rdname scale_coltab
#' @export
get_coltab_pal <- function(x) {
  if (!inherits(x, "SpatRaster")) {
    cli::cli_alert_info(
      paste(
        "{.fun tidyterra::get_coltab_pal} only works with",
        "{.cls SpatRaster} objects, not {.cls {class(x)}}.",
        "Returning {.field NULL}"
      )
    )
    return(NULL)
  }

  if (!any(terra::has.colors(x))) {
    cli::cli_alert_info(
      "{.arg x} does not have a color table. Returning {.field NULL}",
    )
    return(NULL)
  }
  # Complete layers with no coltabs
  iter <- seq_len(terra::nlyr(x))[!terra::has.colors(x)]
  if (length(iter) > 0 && min(iter) > 0) {
    for (h in iter) {
      # Assign coltab
      tmpr <- terra::subset(x, h)
      vals <- as.factor(pull(tmpr))
      terra::values(tmpr) <- vals
      df <- as_tibble(terra::cats(tmpr)[[1]])

      coltb <- data.frame(
        t(
          col2rgb(terrain.colors(nrow(df), rev = TRUE),
            alpha = TRUE
          )
        )
      )
      coltbend <- cbind(df[, 1], coltb)
      terra::coltab(tmpr) <- coltbend
      # Substitute layer
      x[[h]] <- tmpr
    }
  }

  lcats <- terra::cats(x)
  # Get active cats by layer
  actcats <- lapply(seq_len(terra::nlyr(x)), function(p) {
    terra::activeCat(x[[p]])
  })

  # Prepare data frame with categories
  lcats <- lapply(seq_len(terra::nlyr(x)), function(i) {
    i_df <- lcats[[i]]
    if (is.null(i_df)) {
      return(NULL)
    }
    actcat <- unlist(actcats[i])
    df <- i_df[, c(1, actcat + 1)]
    names(df) <- c("id", "label")

    df
  })

  names(lcats) <- names(x)
  cats_end <- dplyr::bind_rows(lcats, .id = "layer")

  # Get cols
  cols_alpha_l <- terra::coltab(x)
  cols_alpha_l <- lapply(cols_alpha_l, function(j) {
    if (is.null(j)) {
      return(NULL)
    }

    df <- j[, seq_len(5)]
    names(df) <- c("id", "r", "g", "b", "a")

    df
  })
  names(cols_alpha_l) <- names(x)
  cols_end <- dplyr::bind_rows(cols_alpha_l, .id = "layer")

  # Join and create
  tojoin <- intersect(names(cats_end), names(cols_end))

  finaltab <- dplyr::left_join(cats_end, cols_end, by = tojoin)

  # Create palette
  colfields <- finaltab[, c("r", "g", "b", "a")]

  namedpal <- rgb(tidyr::drop_na(colfields), maxColorValue = 255)

  # Same length than names
  nms <- unique(finaltab[["label"]])

  # Complete NAs with terrain.cols
  if (!identical(length(namedpal), length(nms))) {
    namedpal <- c(namedpal, terrain.colors(length(nms)))
    namedpal <- namedpal[seq_len(length(nms))]
  }

  names(namedpal) <- nms


  namedpal
}
