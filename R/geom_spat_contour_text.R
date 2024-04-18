#' @export
#' @rdname geom_spat_contour
#' @order 2
#'
#' @param label_format One of:
#'   - `NULL` for no labels. This produced the same result than
#'     `geom_spatraster_contour()`.
#'   - A character vector giving labels (must be same length as the breaks
#'     produced by `bins`, `binwidth`, or `breaks`).
#'   - A function that takes the breaks as input and returns labels as output,
#'     as the default setup ([scales::label_number()]).
#' @inheritParams isoband::isolines_grob
#'
geom_spatraster_contour_text <- function(
    mapping = NULL, data, ..., maxcell = 500000, bins = NULL, binwidth = NULL,
    breaks = NULL, size.unit = "mm",
    label_format = scales::label_number(),
    label_placer = isoband::label_placer_minmax(),
    na.rm = TRUE, show.legend = NA, inherit.aes = TRUE) {
  # Is a suggestion so far
  # nocov start
  if (!requireNamespace("isoband", quietly = TRUE)) {
    cli::cli_abort(paste(
      "Package {.pkg isoband} required.",
      "Run {.run install.packages('isoband')}"
    ))
  }
  # nocov end

  if (!inherits(data, "SpatRaster")) {
    cli::cli_abort(paste(
      "{.fun tidyterra::geom_spatraster_contour_text} only works with",
      "{.cls SpatRaster} objects, not {.cls {class(data)}}.",
      "See {.help terra::vect}"
    ))
  }


  # 1. Work with aes ----
  mapping <- override_aesthetics(
    mapping,
    ggplot2::aes(
      spatraster = .data$spatraster,
      # For faceting
      lyr = .data$lyr
    )
  )


  # aes(z=...) would select the layer to plot
  # Extract value of aes(z)

  if ("z" %in% names(mapping)) {
    namelayer <- vapply(mapping, rlang::as_label, character(1))["z"]

    if (!namelayer %in% names(data)) {
      cli::cli_abort(paste("Layer {.val {namelayer}} not found in {.arg data}"))
    }

    # Subset by layer
    data <- terra::subset(data, namelayer)
    # Remove z from aes, would be provided later on the Stat
    mapping <- cleanup_aesthetics(mapping, "z")
  }


  # 2. Check if resample is needed----

  # Check mixed types
  data <- check_mixed_cols(data)

  data <- resample_spat(data, maxcell)

  # 3. Create a nested list with each layer----
  raster_list <- as.list(data)

  # Now create the data frame
  data_tbl <- tibble::tibble(
    spatraster = list(NULL),
    # For faceting: As factors for keeping orders
    lyr = factor(names(data), levels = names(data))
  )

  names(data_tbl$spatraster) <- names(data)

  # Each layer to a row
  for (i in seq_len(terra::nlyr(data))) {
    data_tbl$spatraster[[i]] <- raster_list[[i]]
  }

  # 4. Build layer ----

  crs_terra <- pull_crs(data)

  # Create layer
  layer_spatrast <- ggplot2::layer(
    data = data_tbl,
    mapping = mapping,
    stat = StatTerraSpatRasterContour,
    geom = GeomSpatRasterContourText,
    position = "identity",
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = rlang::list2(
      na.rm = na.rm,
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      size.unit = size.unit,
      label_placer = label_placer,
      label_format = label_format,
      # Extra params
      maxcell = maxcell,
      ...
    )
  )


  # From ggspatial
  # If the SpatRaster has crs add a geom_sf for training scales
  # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
  # using the first layer's CRS as the base CRS for coord_sf().

  if (!is.na(crs_terra)) {
    layer_spatrast <- c(
      layer_spatrast,
      ggplot2::geom_sf(
        data = sf::st_sfc(sf::st_point(),
          crs = crs_terra
        ),
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    )
  }

  layer_spatrast
}


GeomSpatRasterContourText <- ggplot2::ggproto(
  "GeomSpatRasterContourText",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = aes(
    weight = 1, label = "a", colour = "grey35", linewidth = .2,
    # Reduce base size, align with scales
    size = 3.88 * 0.8, linetype = 1, alpha = NA, family = "sans",
    fontface = 1
  ),
  handle_na = function(self, data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    aesthetics <- c(self$required_aes, self$non_missing_aes)
    complete <- stats::complete.cases(data[names(data) %in% aesthetics])
    kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(!kept)} row{?s} containing missing values or values ",
        "outside the scale range ({.fn GeomSpatRasterContour})."
      ))
    }

    data
  },
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, colour = "grey35", linetype = 1,
                        size.unit = "mm", label_format = scales::label_number(),
                        label_placer = isoband::label_placer_minmax()) {
    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]

    # Override label with default level if it was not provided
    if (data$label[1] == "a") data$label <- data$level


    # back to isolines object
    iso <- df_to_isolines(data)

    # transform back to coordinate space
    iso <- lapply(iso, coord$transform, panel_params)


    # Get aes
    col_raw <- get_aes_iso(data, "colour")
    alpha_raw <- get_aes_iso(data, "alpha")
    col <- ggplot2::alpha(col_raw, alpha_raw)
    lwd <- get_aes_iso(data, "linewidth")
    lty <- get_aes_iso(data, "linetype")
    fam <- get_aes_iso(data, "family")
    face <- get_aes_iso(data, "fontface")
    size <- get_aes_iso(data, "size")
    size_unit <- resolve_text_unit(size.unit)

    # Get breaks
    brks <- names(iso)

    # Get label and format
    lab <- get_aes_iso(data, "label")

    if (rlang::is_function(label_format)) {
      lab <- label_format(as.double(lab))
    } else if (is.null(label_format)) {
      # Explicit request to no plot labels, modify label_placer
      label_placer <- isoband::label_placer_none()
    } else {
      # A vector of values, need to check the length
      lab <- label_format

      # Checks
      l_br <- length(brks)
      l_lab <- length(lab)

      if (l_br != l_lab) {
        cli::cli_abort(
          paste0(
            "Number of labels ({l_lab}) must match the ",
            "number of breaks ({l_br})."
          )
        )
      }
    }

    # Implement isolines_grob
    isoband::isolines_grob(iso,
      gp = grid::gpar(
        # Lines
        lwd = lwd * ggplot2::.pt, lty = lty, lineend = lineend,
        linejoin = linejoin, linemitre = linemitre,
        # Fonts
        fontsize = size * size_unit, fontfamily = fam, fontface = face,
        # Common
        col = col
      ),
      breaks = brks,
      labels = lab,
      units = "native",
      label_placer = label_placer
    )
  },
  draw_key = ggplot2::draw_key_path,
)

# Helpers----

df_to_isolines <- function(path_df) {
  iter <- unique(path_df$level)

  end <- lapply(iter, function(x) {
    df <- path_df[path_df$level == x, ]
    df$piece2 <- (df$piece - min(df$piece)) + 1

    lst <- list(rename = list(
      x = df$x,
      y = df$y,
      id = as.integer(df$piece2)
    ))
    names(lst) <- x
    lst
  })
  end <- do.call(c, end)
  class(end) <- c("isolines", "iso")
  end
}

# Get aes from data for isolines
get_aes_iso <- function(x, aesx = "colour") {
  ind <- unique(x$level)
  get_aes <- lapply(ind, function(y) {
    sb <- x[x$level == y, aesx]
    sb[1]
  })

  unlist(get_aes)
}


## from ggplot2 ----

# Trim false values from left and right: keep all values from
# first TRUE to last TRUE
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(
    rep(FALSE, first),
    rep(TRUE, last - first),
    rep(FALSE, length(x) - last)
  )
}

resolve_text_unit <- function(unit) {
  unit <- rlang::arg_match0(unit, c("mm", "pt", "cm", "in", "pc"))
  switch(unit,
    "mm" = ggplot2::.pt,
    "cm" = ggplot2::.pt * 10,
    "in" = 72.27,
    "pc" = 12,
    1
  )
}
