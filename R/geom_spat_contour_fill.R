#' @export
#' @rdname geom_spat_contour
#' @order 3
#'
geom_spatraster_contour_filled <- function(mapping = NULL, data,
                                           ...,
                                           maxcell = 500000,
                                           bins = NULL,
                                           binwidth = NULL,
                                           breaks = NULL,
                                           na.rm = TRUE,
                                           show.legend = NA,
                                           inherit.aes = TRUE,
                                           mask_projection = FALSE) {
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
      "{.fun tidyterra::geom_spatraster_contour_filled} only works with",
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
    lyr = lyr, order = after_stat(level),
    fill = after_stat(level)
  ),
  extra_params = c(
    "maxcell", "bins", "binwidth", "breaks", "na.rm",
    "coord_crs"
  ),
  setup_params = function(data, params) {
    range_lys <- lapply(data$spatraster, terra::minmax)
    params$z.range <- range(unlist(range_lys), na.rm = TRUE, finite = TRUE)
    params
  },
  compute_layer = function(self, data, params, layout) {
    # warn if not using facets
    if (length(unique(data$PANEL)) != length(unique(data$lyr))) {
      nly <- length(unique(data$lyr))
      if (nly > 1) {
        cli::cli_alert_warning(paste(
          cli::style_bold("{.fun tidyterra::geom_spat_countour_filled}:"),
          "Plotting {.field {nly}} overlapping layer{?s}:",
          "{.val {unique(data$lyr)}}. Either:"
        ))
        cli::cli_bullets(
          c(
            " " = "Use {.code facet_wrap(~lyr)} for faceting or",
            " " = paste(
              "Use {.code aes(fill = <name_of_layer>)}",
              "for displaying single layers"
            )
          )
        )
      }
    }
    # add coord to the params, so it can be forwarded to compute_group()
    params$coord_crs <- pull_crs(layout$coord_params$crs)
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(
      data,
      params, layout
    )
  },
  compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL,
                           breaks = NULL, na.rm = FALSE, coord,
                           coord_crs = NA, mask_projection = FALSE) {
    # Extract raster from group
    rast <- data$spatraster[[1]]

    # Reproject if needed
    rast <- reproject_raster_on_stat(rast, coord_crs, mask = mask_projection)
    # To data and prepare
    prepare_iso <- pivot_longer_spat(rast)
    # Keep initial data
    data_rest <- data
    # Don't need spatraster any more and increase size
    # Set to NA
    data_rest$spatraster <- NA

    # Now adjust min and max value, since reprojection may affect vals
    prepare_iso$value <- pmin(max(z.range), prepare_iso$value)
    prepare_iso$value <- pmax(min(z.range), prepare_iso$value)

    # Now create data with values from raster
    names(prepare_iso) <- c("x", "y", "lyr", "z")

    # Port functions from ggplot2
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

    # Re-create data
    # Remove group, we get that from path_df
    data_rest <- remove_columns(data_rest, "group")

    data <- dplyr::left_join(path_df, data_rest, by = "lyr")

    data
  }
)


# Helpers ----

# From ggplot2

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

  label_low <- format(as.numeric(interval_low),
    digits = dig.lab,
    trim = TRUE
  )
  label_high <- format(as.numeric(interval_high),
    digits = dig.lab,
    trim = TRUE
  )

  # from the isoband::isobands() docs:
  # the intervals specifying isobands are closed at their lower boundary
  # and open at their upper boundary
  sprintf("(%s, %s]", label_low, label_high)
}

iso_to_polygon <- function(iso, group = 1, name_layer = NULL) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))

  if (all(lengths == 0)) {
    cli::cli_warn(paste(
      "In",
      cli::style_bold("{.fun tidyterra::geom_spatraster_contour_filled}:"),
      "Zero contours were generated"
    ))
    return(NULL)
  }

  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)

  # Add leading zeros so that groups can be properly sorted
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
