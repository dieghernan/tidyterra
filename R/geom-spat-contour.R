#' Plot `SpatRaster` contours
#'
#' @description
#'
#' These geoms create contours of `SpatRaster` objects. To specify a valid
#' surface, you should specify the layer on `aes(z = layer_name)`, otherwise all
#' the layers are considered for creating contours. See also **Facets**
#' section.
#'
#' The underlying implementation is based on [ggplot2::geom_contour()].
#'
#' `geom_spatraster_contour_text()` creates labeled contours and it is
#' implemented on top of [isoband::isolines_grob()].
#'
#' @export
#' @encoding UTF-8
#'
#' @rdname geom_spat_contour
#' @name geom_spat_contour
#' @seealso
#' [ggplot2::geom_contour()].
#'
#' The \CRANpkg{metR} package also provides a set of alternative functions:
#' - `metR::geom_contour2()`.
#' - `metR::geom_text_contour()` and `metR::geom_label_contour()`.
#' - `metR::geom_contour_tanaka()`.
#'
#' @family ggplot2.utils
#'
#' @inheritSection geom_spatraster Coords
#' @inheritSection geom_spatraster Facets
#' @inheritParams geom_spatraster
#' @inheritParams ggplot2::geom_contour
#' @inheritParams ggplot2::geom_text
#'
#' @returns A \CRANpkg{ggplot2} layer.
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::contour()]
#'
#' @section Aesthetics:
#'
#' `geom_spatraster_contour()` / `geom_spatraster_contour_text()` understands
#' the following aesthetics:
#' - [`alpha`][ggplot2::aes_colour_fill_alpha]
#' - [`colour`][ggplot2::aes_colour_fill_alpha]
#' - [`group`][ggplot2::aes_group_order]
#' - [`linetype`][ggplot2::aes_linetype_size_shape]
#' - [`linewidth`][ggplot2::aes_linetype_size_shape]
#' `geom_spatraster_contour_text()` understands also:
#' - [`size`][ggplot2::aes_linetype_size_shape]
#' - `label`
#' - `family`
#' - `fontface`
#'
#' Additionally, `geom_spatraster_contour_filled()` understands also the
#' following aesthetics, as well as the ones listed above:
#' - [`fill`][ggplot2::aes_colour_fill_alpha]
#' - `subgroup`
#'
#' Check [ggplot2::geom_contour()] for more information on contours and
#' `vignette("ggplot2-specs", package = "ggplot2")` for an overview of the
#' aesthetics.
#'
#' @section Computed variables:
#'
#' These geoms compute some variables internally that are available for use as
#' aesthetics, using (for example) `aes(color = after_stat(<computed>))` (see
#' [ggplot2::after_stat()]).
#' - `after_stat(lyr)`: Name of the layer.
#' - `after_stat(level)`: Height of contour. For contour lines, this is
#'    a numeric vector that represents bin boundaries. For contour bands,
#'    this is an ordered factor that represents bin ranges.
#' - `after_stat(nlevel)`: Height of contour, scaled to maximum of 1.
#' - `after_stat(level_low)`, `after_stat(level_high)`,
#' - `after_stat(level_mid)`: (contour bands only) Lower and upper bin
#'    boundaries for each band, as well as the midpoint between the boundaries.
#'
#' @section Dropped variables:
#' - `z`: After contouring, the `z` values of individual data points are no
#'   longer available.
#'
#' @order 1
#'
#' @examples
#' \donttest{
#'
#' library(terra)
#'
#' # Raster
#' f <- system.file("extdata/volcano2.tif", package = "tidyterra")
#' r <- rast(f)
#'
#' library(ggplot2)
#'
#' ggplot() +
#'   geom_spatraster_contour(data = r)
#'
#' # Labeled
#' ggplot() +
#'   geom_spatraster_contour_text(
#'     data = r, breaks = c(110, 130, 160, 190),
#'     color = "grey10", family = "serif"
#'   )
#'
#' ggplot() +
#'   geom_spatraster_contour(
#'     data = r, aes(color = after_stat(level)),
#'     binwidth = 1,
#'     linewidth = 0.4
#'   ) +
#'   scale_color_gradientn(
#'     colours = hcl.colors(20, "Inferno"),
#'     guide = guide_coloursteps()
#'   ) +
#'   theme_minimal()
#'
#' # Filled with breaks
#' ggplot() +
#'   geom_spatraster_contour_filled(data = r, breaks = seq(80, 200, 10)) +
#'   scale_fill_hypso_d()
#'
#' # Both lines and contours
#' ggplot() +
#'   geom_spatraster_contour_filled(
#'     data = r, breaks = seq(80, 200, 10),
#'     alpha = 0.7
#'   ) +
#'   geom_spatraster_contour(
#'     data = r, breaks = seq(80, 200, 2.5),
#'     color = "grey30",
#'     linewidth = 0.1
#'   ) +
#'   scale_fill_hypso_d()
#' }
#'
geom_spatraster_contour <- function(
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
  check_spatraster(data, "geom_spatraster_contour")

  contour_data <- prepare_spatraster_contour_data(mapping, data, maxcell)
  mapping <- contour_data$mapping
  data_tbl <- contour_data$data
  crs_terra <- contour_data$crs

  # Create the layer.
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
      mask_projection = mask_projection,
      ...
    )
  )

  # From ggspatial.
  # If the SpatRaster has a CRS, add an empty geom_sf() to train scales. This
  # Mimic using the first layer CRS as the base CRS for `coord_sf()`.

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

prepare_spatraster_contour_data <- function(
  mapping,
  data,
  maxcell,
  call = rlang::caller_env()
) {
  mapping <- override_aesthetics(
    mapping,
    ggplot2::aes(
      spatraster = .data$spatraster,
      lyr = .data$lyr
    )
  )

  selected <- select_spatraster_layer(mapping, data, call = call)
  mapping <- selected$mapping
  data <- selected$data

  data <- check_mixed_cols(data)
  data <- resample_spat(data, maxcell)

  raster_list <- as.list(data)
  data_tbl <- tibble::tibble(
    spatraster = list(NULL),
    lyr = factor(names(data), levels = names(data))
  )

  names(data_tbl$spatraster) <- names(data)

  for (i in seq_len(terra::nlyr(data))) {
    data_tbl$spatraster[[i]] <- raster_list[[i]]
  }

  list(
    mapping = mapping,
    data = data_tbl,
    crs = pull_crs(data)
  )
}

# Geom ----
# Provide a `Geom*` that only changes the defaults of `GeomPath`.
# Align with changes in ggplot2 3.4.0 for `geom_sf()`.
GeomSpatRasterContour <- ggplot2::ggproto(
  "GeomSpatRasterContour",
  ggplot2::GeomPath,
  default_aes = aes(
    weight = 1,
    colour = "grey35",
    linewidth = 0.2,
    linetype = 1,
    alpha = NA
  ),
  # Allow using `size` in ggplot2 < 3.4.0.
  non_missing_aes = "size",
  # Tell ggplot2 to perform automatic renaming.
  rename_size = TRUE
)

# Stat ----
StatTerraSpatRasterContour <- ggplot2::ggproto(
  "StatTerraSpatRasterContour",
  ggplot2::Stat,
  required_aes = "spatraster",
  default_aes = ggplot2::aes(lyr = lyr, order = after_stat(level)),
  extra_params = c(
    "maxcell",
    "bins",
    "binwidth",
    "breaks",
    "na.rm",
    "coord_crs",
    "mask_projection"
  ),
  setup_params = function(data, params) {
    range_lys <- lapply(data$spatraster, terra::minmax)
    params$z.range <- range(unlist(range_lys), na.rm = TRUE, finite = TRUE)
    params
  },
  compute_layer = function(self, data, params, layout) {
    # Add coord to params so it can be forwarded to `compute_group()`.
    warn_overlapping_layers(data, "geom_spatraster_contour")
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

    isolines <- xyz_to_isolines(prepare_iso, breaks)
    path_df <- iso_to_path(isolines, data_rest$group[[1]])

    path_df$level <- as.numeric(path_df$level)
    path_df$nlevel <- scales::rescale_max(path_df$level)
    path_df$lyr <- data_rest$lyr[[1]]

    # Re-create data and remove `group`, which comes from `path_df`.
    data_rest <- remove_columns(data_rest, "group")

    data <- dplyr::left_join(path_df, data_rest, by = "lyr")

    # Final cleanup.
    data <- remove_columns(data, ".size")

    data
  }
)

# Helpers ----

# From ggplot2.
allow_lambda <- function(x) {
  if (rlang::is_formula(x)) rlang::as_function(x) else x
}

# From ggplot2.
contour_breaks <- function(
  z_range,
  bins = NULL,
  binwidth = NULL,
  breaks = NULL
) {
  breaks <- allow_lambda(breaks)

  if (is.numeric(breaks)) {
    return(breaks)
  }

  breaks_fun <- scales::fullseq
  if (is.function(breaks)) {
    breaks_fun <- breaks
  }

  # Use pretty bins when no arguments are set.
  if (is.null(bins) && is.null(binwidth)) {
    breaks <- pretty(z_range, 10)
    return(breaks)
  }

  # Use `bins` to calculate `binwidth` when provided.
  if (!is.null(bins)) {
    # Round limits outward so bins span the data range.
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

  # Otherwise, compute breaks from `binwidth`.
  breaks_fun(z_range, binwidth)
}

isoband_z_matrix <- function(data) {
  # Convert a data vector to a raster matrix.
  x_pos <- as.integer(factor(data$x, levels = sort(unique(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique(data$y))))

  nrow <- max(y_pos)
  ncol <- max(x_pos)

  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z

  raster
}
xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = isoband_z_matrix(data),
    levels = breaks
  )
}

iso_to_path <- function(iso, group = 1) {
  lengths <- vapply(iso, function(x) length(x$x), integer(1))

  if (all(lengths == 0)) {
    cli::cli_warn(paste(
      "In",
      "{.fun tidyterra::geom_spatraster_contour}:",
      "zero contours were generated."
    ))
    return(data.frame())
  }

  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)

  # Add leading zeros so groups sort correctly.
  groups <- paste(
    group,
    sprintf("%03d", item_id),
    sprintf("%03d", ids),
    sep = "-"
  )
  groups <- factor(groups)

  tibble::tibble(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = as.integer(groups),
    group = groups,
    .size = length(xs)
  )
}
