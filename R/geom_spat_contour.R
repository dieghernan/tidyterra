#' Plot `SpatRaster` contours
#'
#' @description
#'
#' These geoms create contours of `SpatRaster` objects.  To specify a valid
#' surface, you should specify the layer on `aes(z = layer_name)`, otherwise all
#' the layers would be consider for creating contours. See also **Facets**
#' section.
#'
#' The underlying implementation is based on [ggplot2::geom_contour()].
#'
#' `r lifecycle::badge("experimental")` `geom_spatraster_contour_text()` creates
#' labeled contours and it is implemented on top of [isoband::isolines_grob()].
#'
#' @export
#'
#' @rdname geom_spat_contour
#' @name geom_spat_contour
#' @order 1
#'
#' @inheritParams geom_spatraster
#' @inheritParams ggplot2::geom_contour
#' @inheritParams ggplot2::geom_text
#'
#' @return A \CRANpkg{ggplot2} layer
#' @family ggplot2.utils
#' @seealso
#' [ggplot2::geom_contour()].
#'
#' The \CRANpkg{metR} package also provides a set of alternative functions:
#' - `metR::geom_contour2()`.
#' - `metR::geom_text_contour()` and `metR::geom_label_contour()`.
#' - `metR::geom_contour_tanaka()`.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::contour()]
#'
#' @inheritSection geom_spatraster Coords
#' @inheritSection  geom_spatraster  Facets
#'
#' @section Aesthetics:
#'
#' `geom_spatraster_contour()` / `geom_spatraster_contour_text()` understands
#'  the following aesthetics:
#'  - [`alpha`][ggplot2::aes_colour_fill_alpha]
#'  - [`colour`][ggplot2::aes_colour_fill_alpha]
#'  - [`group`][ggplot2::aes_group_order]
#'  - [`linetype`][ggplot2::aes_linetype_size_shape]
#'  - [`linewidth`][ggplot2::aes_linetype_size_shape]
#' `geom_spatraster_contour_text()` understands also:
#' - [`size`][ggplot2::aes_linetype_size_shape]
#' - `label`
#' - `family`
#' - `fontface`
#'
#' Additionally, `geom_spatraster_contour_filled()` understands also the
#' following aesthetics, as well as the ones listed above:
#'  - [`fill`][ggplot2::aes_colour_fill_alpha]
#'  - `subgroup`
#'
#' Check [ggplot2::geom_contour()] for more info on contours and
#' `vignette("ggplot2-specs", package = "ggplot2")` for an overview of the
#' aesthetics.
#'
#'
#' @section Computed variables:
#'
#' These geom computes internally some variables that are available for use as
#' aesthetics, using (for example) `aes(color = after_stat(<computed>))` (see
#' [ggplot2::after_stat()]).
#'
#'  - `after_stat(lyr)`: Name of the layer.
#'  - `after_stat(level)`: Height of contour. For contour lines, this is numeric
#'    vector that represents bin boundaries. For contour bands, this is an
#'    ordered factor that represents bin ranges.
#'  - `after_stat(nlevel)`: Height of contour, scaled to maximum of 1.
#'  - `after_stat(level_low)`, `after_stat(level_high)`,
#'    `after_stat(level_mid)`: (contour bands only) Lower and upper bin
#'    boundaries for each band, as well the mid point between the boundaries.
#'
#' @section Dropped variables:
#' - `z`: After contouring, the `z` values of individual data points are no
#'   longer available.
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
#'
#' # Labelled
#' ggplot() +
#'   geom_spatraster_contour_text(
#'     data = r, breaks = c(110, 130, 160, 190),
#'     color = "grey10", family = "serif"
#'   )
#'
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
#'     alpha = .7
#'   ) +
#'   geom_spatraster_contour(
#'     data = r, breaks = seq(80, 200, 2.5),
#'     color = "grey30",
#'     linewidth = 0.1
#'   ) +
#'   scale_fill_hypso_d()
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
    cli::cli_abort(paste(
      "Package {.pkg isoband} required.",
      "Run {.run install.packages('isoband')}"
    ))
  }
  # nocov end

  if (!inherits(data, "SpatRaster")) {
    cli::cli_abort(paste(
      "{.fun tidyterra::geom_spatraster_contour} only works with",
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

# Geom ----
# Provide a Geom* that only changes the defaults of GeomPath
# Aligned with changes in ggplot2 3.4.0 for geom_sf
GeomSpatRasterContour <- ggplot2::ggproto(
  "GeomSpatRasterContour",
  ggplot2::GeomPath,
  default_aes = aes(
    weight = 1,
    colour = "grey35",
    linewidth = .2,
    linetype = 1,
    alpha = NA
  ),
  # To allow using size in ggplot2 < 3.4.0
  non_missing_aes = "size",
  # Tell ggplot2 to perform automatic renaming
  rename_size = TRUE
)

# Stat ----
StatTerraSpatRasterContour <- ggplot2::ggproto(
  "StatTerraSpatRasterContour",
  ggplot2::Stat,
  required_aes = "spatraster",
  default_aes = ggplot2::aes(lyr = lyr, order = after_stat(level)),
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
          cli::style_bold("{.fun tidyterra::geom_spat_countour}:"),
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
                           coord_crs = NA) {
    # Extract raster from group
    rast <- data$spatraster[[1]]

    # Reproject if needed
    rast <- reproject_raster_on_stat(rast, coord_crs)
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

    isolines <- xyz_to_isolines(prepare_iso, breaks)
    path_df <- iso_to_path(isolines, data_rest$group[[1]])

    path_df$level <- as.numeric(path_df$level)
    path_df$nlevel <- scales::rescale_max(path_df$level)
    path_df$lyr <- data_rest$lyr[[1]]

    # Re-create data
    # Remove group, we get that from path_df
    data_rest <- remove_columns(data_rest, "group")

    data <- dplyr::left_join(path_df, data_rest, by = "lyr")

    # Final cleanup
    data <- remove_columns(data, ".size")

    data
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

isoband_z_matrix <- function(data) {
  # Convert vector of data to raster
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
      "In", cli::style_bold("{.fun tidyterra::geom_spatraster_contour}:"),
      "Zero contours were generated"
    ))
    return(data.frame())
  }

  levels <- names(iso)
  xs <- unlist(lapply(iso, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(iso, "[[", "y"), use.names = FALSE)
  ids <- unlist(lapply(iso, "[[", "id"), use.names = FALSE)
  item_id <- rep(seq_along(iso), lengths)

  # Add leading zeros so that groups can be properly sorted
  groups <- paste(group, sprintf("%03d", item_id),
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
