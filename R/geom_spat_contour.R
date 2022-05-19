#' Plot SpatRaster contours
#'
#' @description
#'
#' These geoms create contours of SpatRaster objects.
#'
#' The underlying implementation is based on [ggplot2::geom_contour()].
#'
#' @export
#'
#' @rdname geom_spat_contour
#' @name geom_spat_contour
#'
#' @inheritParams geom_spatraster
#' @param bins Number of contour bins. Overridden by `binwidth`.
#' @param binwidth The width of the contour bins. Overridden by `breaks`.
#' @param breaks One of:
#'   - Numeric vector to set the contour breaks
#'   - A function that takes the range of the data and binwidth as input
#'   and returns breaks as output. A function can be created from a formula
#'   (e.g. ~ fullseq(.x, .y)).
#'
#'   Overrides `binwidth` and `bins`. By default, this is a vector of length
#'   ten with [pretty()] breaks.
#'
#' @return A ggplot2 layer
#' @family ggplot2.utils
#' @seealso  [ggplot2::geom_contour()]
#'
#' @section  terra equivalent:
#'
#' [terra::contour()]
#'
#' @inheritSection geom_spatraster Coords
#' @inheritSection  geom_spatraster  Facets
#'
#' @section Aesthetics:
#'
#' `geom_spatraster_contour`() understands the following aesthetics:
#'
#'  - alpha
#'  - colour
#'  - linetype
#'  - size
#'
#' Additionally, `geom_spatraster_contour_filled()` understands also the
#' following aesthetics:
#'  - fill
#'
#' Check [ggplot2::geom_contour()] for more info.
#'
#'
#' @section Computed variables:
#'
#' This geom computes internally some variables that are available for use as
#' aesthetics, using (for example) `aes(color = after_stat(<computed>))` (see
#' [ggplot2::after_stat()]).
#'
#' \describe{
#'  \item{`level`}{Height of contour. For contour lines, this is numeric vector
#'    that represents bin boundaries. For contour bands, this is an ordered
#'    factor that represents bin ranges.}
#'  \item{`nlevel`}{Height of contour, scaled to maximum of 1.}
#'  \item{`lyr`}{Name of the layers.}
#'  \item{`level_low`, `level_high`, `level_mid`}{(contour bands only) Lower
#'    and upper bin boundaries for each band, as well the mid point between
#'    the boundaries.}
#' }
#'
#' @examples
#' \donttest{
#' library(terra)
#'
#' # Raster
#' f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#' r <- rast(f)
#'
#' library(ggplot2)
#'
#' ggplot() +
#'   geom_spatraster_contour(data = r) +
#'   facet_wrap(~lyr)
#'
#' # Select one layer
#'
#' ggplot() +
#'   geom_spatraster_contour(data = r, aes(z = tavg_04))
#' # With aes on computed variable
#'
#' ggplot() +
#'   geom_spatraster_contour(
#'     data = r, aes(color = after_stat(level)),
#'     binwidth = 1,
#'     size = 0.4
#'   ) +
#'   facet_wrap(~lyr) +
#'   scale_color_gradientn(
#'     colours = hcl.colors(20, "Inferno"),
#'     guide = guide_coloursteps()
#'   ) +
#'   theme_minimal()
#'
#' # Filled with breaks
#' ggplot() +
#'   geom_spatraster_contour_filled(data = r, breaks = seq(5, 20, 2.5)) +
#'   facet_wrap(~lyr)
#'
#' # Both lines and contours aligned with breaks
#' ggplot() +
#'   geom_spatraster_contour_filled(
#'     data = r, breaks = seq(5, 20, 2.5),
#'     alpha = .7
#'   ) +
#'   geom_spatraster_contour(
#'     data = r, breaks = seq(5, 20, 2.5),
#'     color = "black"
#'   ) +
#'   facet_wrap(~lyr)
#' }
#'
geom_spatraster_contour <- function(mapping = NULL, data,
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
    geom = GeomSpatRasterContour,
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


# Geom----

# Provide a Geom* that only changes the defaults of GeomPath
GeomSpatRasterContour <- ggplot2::ggproto(
  "GeomSpatRasterContour",
  ggplot2::GeomPath,
  default_aes = aes(
    weight = 1,
    colour = "grey50",
    size = .5,
    linetype = 1,
    alpha = NA
  )
)


# Stat ----

StatTerraSpatRasterContour <- ggplot2::ggproto(
  "StatTerraSpatRasterContour",
  ggplot2::Stat,
  required_aes = "spatraster",
  default_aes = ggplot2::aes(
    order = stat(level),
    group = stat(group),
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

    # Make path for each layer
    path_list <- lapply(rast, layer_to_path, breaks = breaks)

    if (all(vapply(path_list, is.null, logical(1)))) {

      # No contours created
      # Return empty data frame
      return(data.frame(matrix(ncol = 0, nrow = 0)))
    }

    path_df <- dplyr::bind_rows(path_list)

    # Remove cols that I dont need here
    data <- remove_columns(data, c("spatraster", "group"))

    path_df <- dplyr::left_join(path_df, data, by = "lyr")

    path_df$level <- as.numeric(path_df$level)
    path_df$nlevel <- scales::rescale_max(path_df$level)

    path_df
  }
)


# Helpers ----

# From ggplot2
allow_lambda <- function(x) {
  if (rlang::is_formula(x)) rlang::as_function(x) else x
}

# From ggplot2
contour_breaks <- function(z_range, bins = NULL, binwidth = NULL,
                           breaks = NULL) {
  breaks <- allow_lambda(breaks)

  if (is.numeric(breaks)) {
    return(breaks)
  }

  breaks_fun <- scales::fullseq
  if (is.function(breaks)) {
    breaks_fun <- breaks
  }

  # If no parameters set, use pretty bins
  if (is.null(bins) && is.null(binwidth)) {
    breaks <- pretty(z_range, 10)
    return(breaks)
  }

  # If provided, use bins to calculate binwidth
  if (!is.null(bins)) {
    # round lower limit down and upper limit up to make sure
    # we generate bins that span the data range nicely
    accuracy <- signif(diff(z_range), 1) / 10
    z_range[1] <- floor(z_range[1] / accuracy) * accuracy
    z_range[2] <- ceiling(z_range[2] / accuracy) * accuracy

    if (bins == 1) {
      return(z_range)
    }

    binwidth <- diff(z_range) / (bins - 1)
    breaks <- breaks_fun(z_range, binwidth)

    # Sometimes the above sequence yields one bin too few.
    # If this happens, try again.
    if (length(breaks) < bins + 1) {
      binwidth <- diff(z_range) / bins
      breaks <- breaks_fun(z_range, binwidth)
    }

    return(breaks)
  }

  # if we haven't returned yet, compute breaks from binwidth
  breaks_fun(z_range, binwidth)
}

layer_to_path <- function(rast, breaks) {
  df <- as_tibble(rast, xy = TRUE, na.rm = FALSE)
  name_layer <- names(rast)

  # Get matrix from raster
  m <- terra::as.matrix(rast,
    wide = TRUE
  )
  m[is.na(m)] <- NA

  # Need to reverse (¿?)
  m <- m[rev(seq_len(nrow(m))), ]


  iso_lines <- isoband::isolines(
    x = sort(unique(df$x)),
    y = sort(unique(df$y)),
    z = m,
    levels = breaks
  )

  # Using sf conversion
  lines_sf <- isoband::iso_to_sfg(iso_lines)
  lines_sf <- sf::st_sf(
    level = names(lines_sf),
    geometry = sf::st_sfc(lines_sf)
  )

  # Remove empty geoms
  lines_sf <- lines_sf[!sf::st_is_empty(lines_sf), ]


  if (isFALSE(nrow(lines_sf) > 1)) {
    warning("geom_spatraster_contour(): ",
      "Zero contours were generated for layer ",
      name_layer,
      call. = FALSE
    )

    return(NULL)
  }

  # Make pieces
  to_pieces <- sf::st_cast(lines_sf, "LINESTRING",
    ids = lines_sf$level, warn = FALSE
  )

  to_pieces$piece <- seq_len(nrow(to_pieces))
  to_pieces$lyr <- name_layer
  to_pieces_df <- sf::st_drop_geometry(to_pieces)
  to_pieces_coords <- as.data.frame(sf::st_coordinates(to_pieces))
  names(to_pieces_coords) <- c("x", "y", "piece")

  # Final datatset
  final_df <- dplyr::left_join(to_pieces_df, to_pieces_coords, by = "piece")
  final_df <- final_df[, c("level", "x", "y", "piece", "lyr")]

  # Make groups
  level_to_integer <- as.integer(factor(final_df$level, levels = breaks))
  final_df$group <- paste(name_layer,
    sprintf("%03d", level_to_integer),
    sprintf("%03d", final_df$piece),
    sep = "_"
  )

  final_df
}
