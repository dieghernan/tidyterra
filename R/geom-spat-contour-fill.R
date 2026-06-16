#' @export
#' @encoding UTF-8
#' @rdname geom_spat_contour
#' @order 3
#'
geom_spatraster_contour_filled <- function(
  mapping = NULL,
  data,
  ...,
  maxcell = 500000,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  mask_projection = FALSE
) {
  check_spatraster(data, "geom_spatraster_contour_filled")

  contour_data <- prepare_spatraster_contour_data(mapping, data, maxcell)
  mapping <- contour_data$mapping
  data_tbl <- contour_data$data
  crs_terra <- contour_data$crs

  # Create the layer.
  layer_spatrast <- ggplot2::layer(
    data = data_tbl,
    mapping = mapping,
    stat = StatTerraSpatRasterContourFill,
    geom = GeomSpatRasterContourFilled,
    position = "identity",
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = rlang::list2(
      na.rm = na.rm,
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
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

# Geom ----
GeomSpatRasterContourFilled <- ggplot2::ggproto(
  "GeomSpatRasterContourFilled",
  ggplot2::GeomPolygon,
  default_aes = aes(
    colour = NA,
    fill = "grey90",
    linewidth = 0.2,
    linetype = 1,
    alpha = NA,
    subgroup = NULL
  )
)

# Stat ----

StatTerraSpatRasterContourFill <- ggplot2::ggproto(
  "StatTerraSpatRasterContourFill",
  ggplot2::Stat,
  required_aes = "spatraster",
  default_aes = ggplot2::aes(
    lyr = lyr,
    order = after_stat(level),
    fill = after_stat(level)
  ),
  extra_params = c(
    "maxcell",
    "bins",
    "binwidth",
    "breaks",
    "na.rm",
    "coord_crs"
  ),
  setup_params = function(data, params) {
    range_lys <- lapply(data$spatraster, terra::minmax)
    params$z.range <- range(unlist(range_lys), na.rm = TRUE, finite = TRUE)
    params
  },
  compute_layer = function(self, data, params, layout) {
    # Add coord to params so it can be forwarded to `compute_group()`.
    warn_overlapping_layers(data, "geom_spatraster_contour_filled")
    params$coord_crs <- pull_crs(layout$coord_params$crs)
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(
      data,
      params,
      layout
    )
  },
  compute_group = function(
    data,
    scales,
    z.range,
    bins = NULL,
    binwidth = NULL,
    breaks = NULL,
    na.rm = FALSE,
    coord,
    coord_crs = NA,
    mask_projection = FALSE
  ) {
    # Extract the raster from the current group.
    rast <- data$spatraster[[1]]

    # Reproject if needed.
    rast <- reproject_raster_on_stat(rast, coord_crs, mask = mask_projection)
    # Convert to a data frame and prepare output.
    prepare_iso <- pivot_longer_spat(rast)
    # Keep the initial data.
    data_rest <- data
    # Drop the raster payload before joining to reduce the output size.
    data_rest$spatraster <- NA

    # Adjust minimum and maximum values because reprojection may affect them.
    prepare_iso$value <- pmin(max(z.range), prepare_iso$value)
    prepare_iso$value <- pmax(min(z.range), prepare_iso$value)

    # Create data with values from the raster.
    names(prepare_iso) <- c("x", "y", "lyr", "z")

    # Reuse contour break logic from ggplot2.
    breaks <- contour_breaks(z.range, bins, binwidth, breaks)

    isobands <- xyz_to_isobands(prepare_iso, breaks)
    names(isobands) <- pretty_isoband_levels(names(isobands))
    path_df <- iso_to_polygon(isobands, data_rest$group[[1]])

    path_df$level <- ordered(path_df$level, levels = names(isobands))
    path_df$level_low <- breaks[as.numeric(path_df$level)]
    path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
    path_df$level_mid <- 0.5 * (path_df$level_low + path_df$level_high)
    path_df$nlevel <- scales::rescale_max(path_df$level_high)
    path_df$lyr <- data_rest$lyr[[1]]

    # Re-create data and remove `group`, which comes from `path_df`.
    data_rest <- remove_columns(data_rest, "group")

    data <- dplyr::left_join(path_df, data_rest, by = "lyr")

    data
  }
)

# Helpers ----

# From ggplot2.

xyz_to_isobands <- function(data, breaks) {
  isoband::isobands(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = isoband_z_matrix(data),
    levels_low = breaks[-length(breaks)],
    levels_high = breaks[-1]
  )
}

pretty_isoband_levels <- function(isoband_levels, dig.lab = 3) {
  interval_low <- gsub(":.*$", "", isoband_levels)
  interval_high <- gsub("^[^:]*:", "", isoband_levels)

  label_low <- format(as.numeric(interval_low), digits = dig.lab, trim = TRUE)
  label_high <- format(as.numeric(interval_high), digits = dig.lab, trim = TRUE)

  # From the `isoband::isobands()` docs: isoband intervals are closed at their
  # lower boundary and open at their upper boundary.
  sprintf("(%s, %s]", label_low, label_high)
}

iso_to_polygon <- function(iso, group = 1, name_layer = NULL) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))

  if (all(lengths == 0)) {
    cli::cli_warn(paste(
      "In",
      "{.fun tidyterra::geom_spatraster_contour_filled}:",
      "zero contours were generated."
    ))
    return(NULL)
  }

  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)

  # Add leading zeros so groups sort correctly.
  groups <- paste(group, sprintf("%03d", item_id), sep = "-")
  groups <- factor(groups)

  df <- data.frame(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = as.integer(groups),
    group = groups,
    subgroup = ids
  )

  df$lyr <- name_layer

  df
}
