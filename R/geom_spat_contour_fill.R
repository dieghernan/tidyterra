#' @export
#' @rdname geom_spat_contour
#'
geom_spatraster_contour_filled <- function(mapping = NULL, data,
                                           ...,
                                           maxcell = 500000,
                                           bins = NULL,
                                           binwidth = NULL,
                                           breaks = NULL,
                                           na.rm = TRUE,
                                           show.legend = NA,
                                           inherit.aes = TRUE) {

  # Is a suggestion so far

  # nocov start
  if (!requireNamespace("isoband", quietly = TRUE)) {
    stop("Package `isoband` required. Run `install.packages('isoband')` first")
  }
  # nocov end

  if (!inherits(data, "SpatRaster")) {
    stop(
      "geom_spatraster_*() only works with SpatRaster objects. ",
      "See ?terra::vect"
    )
  }


  # 1. Work with aes ----
  mapping <- override_aesthetics(
    mapping,
    ggplot2::aes_string(
      spatraster = "spatraster",
      # For faceting
      lyr = "lyr"
    )
  )


  # aes(z=...) would select the layer to plot
  # Extract value of aes(z)

  if ("z" %in% names(mapping)) {
    namelayer <- vapply(mapping, rlang::as_label, character(1))["z"]

    if (!namelayer %in% names(data)) {
      cli::cli_abort(paste("Layer", namelayer, "not found in data"))
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
    geom = ggplot2::GeomContourFilled,
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


# Stat ----

StatTerraSpatRasterContourFill <- ggplot2::ggproto(
  "StatTerraSpatRasterContourFill",
  ggplot2::Stat,
  required_aes = "spatraster",
  default_aes = ggplot2::aes(
    order = stat(level),
    fill = stat(level),
    lyr = lyr
  ),
  extra_params = c("maxcell", "bins", "binwidth", "breaks"),
  compute_layer = function(self, data, params, layout) {

    # Need to do is on the layer for assessing breaks
    # after projecting

    # If no faceting or not single layer
    if (nrow(data) != length(unique(data$PANEL))) {
      message(
        "\nWarning message:\n",
        "Plotting ", nrow(data), " layers: ",
        paste0("`", unique(data$lyr), "`", collapse = ", "),
        ".(geom_spatraster_*()).",
        "\n- Use facet_wrap(~lyr) for faceting.",
        "\n- Use aes(z=<name_of_layer>) ",
        "for displaying a single layer\n"
      )
    }


    # On SpatRaster with crs check if need to reproject

    # Project on list
    rast <- lapply(data$spatraster, function(x) {
      reproject_raster_on_stat(
        x,
        pull_crs(layout$coord_params$crs)
      )
    })

    # Prepare breaks
    range_lys <- lapply(rast, terra::minmax)
    z.range <- range(unlist(range_lys), na.rm = TRUE, finite = TRUE)

    breaks <- contour_breaks(
      z.range, params$bins,
      params$binwidth,
      params$breaks
    )

    # Make polygon for each layer
    polygon_list <- lapply(rast, layer_to_polygon, breaks = breaks)

    if (all(vapply(polygon_list, is.null, logical(1)))) {
      # No contours created
      # Return empty data frame
      return(data.frame(matrix(ncol = 0, nrow = 0)))
    }

    path_df <- dplyr::bind_rows(polygon_list)


    # Extract for levels
    for_levels <- dplyr::distinct(
      path_df[, c("level", "left_interval")]
    )

    for_levels <- for_levels[order(for_levels$left_interval), ]
    path_df$level <- ordered(path_df$level,
      levels = for_levels$level
    )

    path_df <- remove_columns(path_df, c("left_interval"))

    # Remove cols that I dont need here
    data <- remove_columns(data, c("spatraster", "group"))


    path_df <- dplyr::left_join(path_df, data, by = "lyr")

    path_df$level_low <- breaks[as.numeric(path_df$level)]
    path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
    path_df$level_mid <- 0.5 * (path_df$level_low + path_df$level_high)
    path_df$nlevel <- scales::rescale_max(path_df$level_high)

    path_df
  }
)


# Helpers ----
layer_to_polygon <- function(rast, breaks) {
  df <- as_tibble(rast, xy = TRUE, na.rm = FALSE)
  name_layer <- names(rast)

  # Get matrix from raster

  m <- terra::as.matrix(rast, wide = TRUE)

  m[is.na(m)] <- NA

  # Need to reverse
  m <- m[rev(seq_len(nrow(m))), ]

  iso <- isoband::isobands(
    x = sort(unique(df$x)),
    y = sort(unique(df$y)),
    z = m,
    levels_low = breaks[-length(breaks)],
    levels_high = breaks[-1]
  )

  # Data frame for ordering levels
  df_levels <- data.frame(level = pretty_isoband_levels(names(iso)))

  df_levels$left_interval <- vapply(names(iso), function(x) {
    as.numeric(unlist(strsplit(x, ":"))[1])
  },
  FUN.VALUE = numeric(1), USE.NAMES = FALSE
  )

  names(iso) <- pretty_isoband_levels(names(iso))

  final_df <- iso_to_polygon(iso,
    group = name_layer,
    name_layer = name_layer
  )

  if (is.null(final_df)) {
    return(NULL)
  }

  final_df <- dplyr::left_join(final_df, df_levels, by = "level")

  final_df
}

# From ggplot2

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
    warning(
      "spatraster_contour(): Zero contours were generated for layer ",
      name_layer,
      call. = FALSE
    )
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
