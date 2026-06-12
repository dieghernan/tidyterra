#' @export
#' @encoding UTF-8
#' @rdname geom_spat_contour
#' @inheritParams isoband::isolines_grob
#'
#' @param label_format One of:
#'   - `NULL` for no labels. This produces the same result as
#'     `geom_spatraster_contour()`.
#'   - A character vector giving labels (must have the same length as the breaks
#'     produced by `bins`, `binwidth` or `breaks`).
#'   - A function that takes the breaks as input and returns labels as output,
#'     as the default setup ([scales::label_number()]).
#' @order 2
#'
geom_spatraster_contour_text <- function(
  mapping = NULL,
  data,
  ...,
  maxcell = 500000,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  size.unit = "mm",
  label_format = scales::label_number(),
  label_placer = isoband::label_placer_minmax(),
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  mask_projection = FALSE
) {
  check_spatraster(data, "geom_spatraster_contour_text")

  contour_data <- prepare_spatraster_contour_data(mapping, data, maxcell)
  mapping <- contour_data$mapping
  data_tbl <- contour_data$data
  crs_terra <- contour_data$crs

  # Create the layer.
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
      mask_projection = mask_projection,
      ...
    )
  )

  # From ggspatial.
  # If the `SpatRaster` has a CRS, add an empty `geom_sf()` to train scales.
  # This mimics using the first layer CRS as the base CRS for `coord_sf()`.

  if (!is.na(crs_terra)) {
    layer_spatrast <- c(
      layer_spatrast,
      ggplot2::geom_sf(
        data = sf::st_sfc(sf::st_point(), crs = crs_terra),
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
    weight = 1,
    label = "a",
    colour = "grey35",
    linewidth = 0.2,
    # Reduce base size and align with scales.
    size = 3.88 * 0.8,
    linetype = 1,
    alpha = NA,
    family = "sans",
    fontface = 1
  ),
  handle_na = function(self, data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # Use the middle point, where a line break is expected.
    aesthetics <- c(self$required_aes, self$non_missing_aes)
    complete <- stats::complete.cases(data[names(data) %in% aesthetics])
    kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(!kept)} row{?s} containing missing values or values ",
        "outside the scale range ({.fn GeomSpatRasterContourText})."
      ))
    }

    data
  },
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    arrow = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE,
    colour = "grey35",
    linetype = 1,
    size.unit = "mm",
    label_format = scales::label_number(),
    label_placer = isoband::label_placer_minmax()
  ) {
    # Must be sorted by group.
    data <- data[order(data$group), , drop = FALSE]

    # Override label with the default level when it was not provided.
    if (data$label[1] == "a") {
      data$label <- data$level
    }

    # Convert back to an isolines object.
    iso <- df_to_isolines(data)

    # Transform back to coordinate space.
    iso <- lapply(iso, coord$transform, panel_params)

    # Get aesthetics.
    col_raw <- get_aes_iso(data, "colour")
    alpha_raw <- get_aes_iso(data, "alpha")
    col <- ggplot2::alpha(col_raw, alpha_raw)
    lwd <- get_aes_iso(data, "linewidth")
    lty <- get_aes_iso(data, "linetype")
    fam <- get_aes_iso(data, "family")
    face <- get_aes_iso(data, "fontface")
    size <- get_aes_iso(data, "size")
    size_unit <- resolve_text_unit(size.unit)

    # Get breaks.
    brks <- names(iso)

    # Get labels and format.
    lab <- get_aes_iso(data, "label")

    if (rlang::is_function(label_format)) {
      lab <- label_format(as.double(lab))
    } else if (is.null(label_format)) {
      # Do not plot labels when explicitly requested.
      label_placer <- isoband::label_placer_none()
    } else {
      # Check the length of a value vector.
      lab <- label_format

      # Check label and break lengths.
      l_br <- length(brks)
      l_lab <- length(lab)

      if (l_br != l_lab) {
        cli::cli_abort(paste0(
          "Number of labels ({l_lab}) must match the ",
          "number of breaks ({l_br})."
        ))
      }
    }

    # Draw the isolines grob.
    isoband::isolines_grob(
      iso,
      gp = grid::gpar(
        # Lines
        lwd = lwd * ggplot2::.pt,
        lty = lty,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        # Fonts
        fontsize = size * size_unit,
        fontfamily = fam,
        fontface = face,
        # Common
        col = col
      ),
      breaks = brks,
      labels = lab,
      units = "native",
      label_placer = label_placer
    )
  },
  draw_key = ggplot2::draw_key_path
)

# Helpers----

df_to_isolines <- function(path_df) {
  iter <- unique(path_df$level)

  end <- lapply(iter, function(x) {
    df <- path_df[path_df$level == x, ]
    df$piece2 <- (df$piece - min(df$piece)) + 1

    lst <- list(rename = list(x = df$x, y = df$y, id = as.integer(df$piece2)))
    names(lst) <- x
    lst
  })
  end <- do.call(c, end)
  class(end) <- c("isolines", "iso")
  end
}

# Get aesthetics from data for isolines.
get_aes_iso <- function(x, aesx = "colour") {
  ind <- unique(x$level)
  get_aes <- lapply(ind, function(y) {
    sb <- x[x$level == y, aesx]
    sb[1]
  })

  unlist(get_aes)
}

## From ggplot2 ----

# Trim false values from the left and right, keeping all values from the first
# `TRUE` to the last `TRUE`.
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(rep(FALSE, first), rep(TRUE, last - first), rep(FALSE, length(x) - last))
}

resolve_text_unit <- function(unit) {
  unit <- rlang::arg_match(unit, values = c("mm", "pt", "cm", "in", "pc"))
  switch(unit,
    "mm" = ggplot2::.pt,
    "cm" = ggplot2::.pt * 10,
    "in" = 72.27,
    "pc" = 12,
    1
  )
}
