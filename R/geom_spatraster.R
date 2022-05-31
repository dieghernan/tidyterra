#' Visualise SpatRaster objects
#'
#' @description
#'
#' This geom is used to visualise SpatRaster objects (see [terra::rast()]). The
#' geom is designed for visualise the object by layers, as [terra::plot()] does.
#'
#' For plotting SpatRaster objects as map tiles (i.e. RGB SpatRaster), use
#' [geom_spatraster_rgb()].
#'
#' The underlying implementation is based on [ggplot2::geom_raster()].
#'
#' @return A ggplot2 layer
#' @family ggplot2.utils
#' @param data A SpatRaster object.
#'
#' @source Based on the `layer_spatial()` implementation on ggspatial package.
#' Thanks to [Dewey Dunnington](https://github.com/paleolimbot) and
#' [ggspatial contributors](https://github.com/paleolimbot/ggspatial/graphs/contributors).
#'
#' @param maxcell positive integer. Maximum number of cells to use for
#'   the plot.
#' @inheritParams ggplot2::geom_raster
#'
#' @seealso [ggplot2::geom_raster()], [ggplot2::coord_sf()],
#' [ggplot2::facet_wrap()]
#'
#' @section  terra equivalent:
#'
#' [terra::plot()]
#'
#' @section Coords:
#'
#' When the SpatRaster does not present a crs (i.e.,
#' `terra::crs(rast) == ""`) the geom does not make any assumption on the
#' scales.
#'
#' On SpatRaster that have a crs, the geom uses [ggplot2::coord_sf()] to adjust
#' the scales. That means that also the **SpatRaster may be reprojected**.
#'
#'
#' @section Aesthetics:
#'
#'
#' `geom_spatraster()` understands the following aesthetics:
#'
#' - `fill`
#'
#' If `fill` is not provided, `geom_spatraster()` creates a ggplot2 layer with
#' all the layers of the SpatRaster object. Use `facet_wrap(~lyr)` to display
#' properly the SpatRaster layers.
#'
#' If `fill` is used, it should contain the name of one layer that is present
#' on the SpatRaster (i.e.
#' `geom_spatraster(data = rast, aes(fill = <name_of_lyr>)`). Names of the
#' layers can be retrieved using `names(rast)`.
#'
#'
#' @section Facets:
#'
#' You can use ` facet_wrap(~lyr)` for creating a faceted plot by each layer of
#' the SpatRaster object. See [ggplot2::facet_wrap()] for details.
#'
#' @section Computed variables:
#'
#' This geom computes internally some variables that are available for use as
#' aesthetics, using (for example) `aes(color = after_stat(<computed>))` (see
#' [ggplot2::after_stat()]).
#'
#' \describe{
#'  \item{`lyr`}{Name of the layer.}
#' }
#'
#' @export
#' @examples
#' \donttest{
#' # Avg temperature on spring in Castille and Leon (Spain)
#' file_path <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#'
#' library(terra)
#' temp_rast <- rast(file_path)
#'
#' library(ggplot2)
#'
#' # Display a single layer
#' names(temp_rast)
#'
#' ggplot() +
#'   geom_spatraster(data = temp_rast, aes(fill = tavg_04)) +
#'   # You can use coord_sf
#'   coord_sf(crs = 3857) +
#'   scale_fill_hypso_c()
#'
#' # Display facets
#' ggplot() +
#'   geom_spatraster(data = temp_rast) +
#'   facet_wrap(~lyr, ncol = 2) +
#'   scale_fill_hypso_b()
#'
#'
#' # Non spatial rasters
#'
#' no_crs <- rast(crs = NA, extent = c(0, 100, 0, 100), nlyr = 1)
#' values(no_crs) <- seq_len(ncell(no_crs))
#'
#'
#' ggplot() +
#'   geom_spatraster(data = no_crs)
#'
#' # Downsample
#'
#' ggplot() +
#'   geom_spatraster(data = no_crs, maxcell = 25)
#' }
#'
geom_spatraster <- function(mapping = aes(),
                            data,
                            na.rm = TRUE,
                            show.legend = NA,
                            inherit.aes = FALSE,
                            interpolate = FALSE,
                            maxcell = 500000,
                            ...) {
  if (!inherits(data, "SpatRaster")) {
    stop(
      "geom_spatraster() only works with SpatRaster objects. ",
      "See ?terra::vect"
    )
  }


  # 1. Work with aes ----

  mapping <- cleanup_aesthetics(mapping, "group")

  mapping <- override_aesthetics(
    mapping,
    ggplot2::aes_string(
      spatraster = "spatraster",
      # For faceting
      lyr = "lyr",
      group = "lyr"
    )
  )


  # aes(fill=...) would select the layer to plot
  # Extract value of aes(fill)

  if ("fill" %in% names(mapping)) {
    namelayer <- vapply(mapping, rlang::as_label, character(1))["fill"]

    if (!namelayer %in% names(data)) {
      cli::cli_abort(paste("Layer", namelayer, "not found in data"))
    }

    # Subset by layer
    data <- terra::subset(data, namelayer)
    # Remove fill from aes, would be provided later on the Stat
    mapping <- cleanup_aesthetics(mapping, "fill")
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
    stat = StatTerraSpatRaster,
    geom = ggplot2::GeomRaster,
    position = "identity",
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(
      na.rm = na.rm,
      # Extra params
      maxcell = maxcell,
      interpolate = interpolate,
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

StatTerraSpatRaster <- ggplot2::ggproto(
  "StatTerraSpatRaster",
  ggplot2::Stat,
  required_aes = "spatraster",
  default_aes = ggplot2::aes(
    fill = stat(value), lyr = lyr, group = lyr,
    spatraster = stat(spatraster)
  ),
  extra_params = c("maxcell", "na.rm", "coord_crs"),
  compute_layer = function(self, data, params, layout) {
    # warn if not using facets
    if (length(unique(data$PANEL)) != length(unique(data$lyr))) {
      nly <- length(unique(data$lyr))
      message(
        "\nWarning message:\n",
        "Plotting ", nly, " layers: ",
        paste0("`", unique(data$lyr), "`", collapse = ", "),
        ".(geom_spatraster).",
        "\n- Use facet_wrap(~lyr) for faceting.",
        "\n- Use aes(fill=<name_of_layer>) ",
        "for displaying a single layer\n"
      )
    }
    # add coord to the params, so it can be forwarded to compute_group()
    params$coord_crs <- pull_crs(layout$coord_params$crs)
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(
      data,
      params, layout
    )
  },
  compute_group = function(data, scales, coord, params,
                           coord_crs = NA) {

    # Extract raster from group
    rast <- data$spatraster[[1]]

    # Reproject if needed
    rast <- reproject_raster_on_stat(rast, coord_crs)

    # To data and prepare
    data_end <- pivot_longer_spat(rast)
    data_rest <- data

    # Don't need spatraster any more and increase size
    # Set to NA
    data_rest$spatraster <- NA

    data <- dplyr::left_join(data_end, data_rest, by = "lyr")
    data
  }
)


# Helpers ----

# Remove column names
remove_columns <- function(x, rem) {
  tokeep <- setdiff(names(x), rem)
  clean <- x[, tokeep]
  clean
}

# Reproject a SpatRaster with params
reproject_raster_on_stat <- function(raster,
                                     coords_crs = NA) {

  # Check if need to reproject
  crs_terra <- pull_crs(raster)

  # If no CRS no reprojection
  if (is.na(crs_terra)) {
    return(raster)
  }

  coord_crs <- pull_crs(coords_crs)

  if (is.na(coord_crs)) {
    cli::cli_abort(
      paste(
        "geom_spatraster_*() on SpatRasters with crs",
        "must be used with coord_sf()."
      ),
      call. = TRUE
    )
  }

  # On equal don't needed
  if (coord_crs == crs_terra) {
    return(raster)
  }
  init_rast <- raster

  # Create template for projection
  template <- terra::project(
    terra::rast(init_rast),
    coord_crs
  )

  # Try to keep the same number of cells on the template
  template <- terra::spatSample(template, terra::ncell(init_rast),
    as.raster = TRUE,
    method = "regular"
  )


  # Reproject
  proj_rast <- terra::project(init_rast, template)

  return(proj_rast)
}

pivot_longer_spat <- function(x) {
  tb <- as_tbl_spat_attr(x)
  tb_pivot <- tidyr::pivot_longer(tb, -c(1:2),
    names_to = "lyr"
  )
  tb_pivot <- tb_pivot[order(tb_pivot$lyr), ]

  tb_pivot
}

# also need a method to combine aesthetics with overriding values
# From ggspatial
override_aesthetics <- function(user_mapping = NULL, default_mapping = NULL) {
  if (is.null(user_mapping) && is.null(default_mapping)) {
    ggplot2::aes()
  } else if (is.null(default_mapping)) {
    user_mapping
  } else if (is.null(user_mapping)) {
    default_mapping
  } else {
    all_aes_names <- unique(c(names(user_mapping), names(default_mapping)))
    new_aes <- c(user_mapping, default_mapping)[all_aes_names]
    class(new_aes) <- "uneval"
    new_aes
  }
}

cleanup_aesthetics <- function(mapping,
                               remove = c("r", "g", "b", "fill")) {
  allaes <- names(mapping)
  keepaes <- setdiff(allaes, remove)
  new_aes <- mapping[keepaes]
  class(new_aes) <- "uneval"
  new_aes
}

resample_spat <- function(r, maxcell = 50000) {
  if (terra::ncell(r) > 1.1 * maxcell) {
    r <- terra::spatSample(r, maxcell,
      as.raster = TRUE,
      method = "regular"
    )
    message("SpatRaster resampled to ncells = ", terra::ncell(r))
  }

  return(r)
}

check_mixed_cols <- function(r) {
  todf <- terra::as.data.frame(r[1], xy = FALSE)
  col_classes <- unlist(lapply(todf, class))

  # If all the same class then do nothing
  if (length(unique(col_classes)) == 1) {
    return(r)
  }

  # If not, select the first class
  final <- col_classes[1]

  # Work with indexes
  extract_vars <- as.integer(which(col_classes == final))
  newr <- terra::subset(r, extract_vars)

  message(
    "Warning message:\n",
    "Mixed data classes found on layers. Only layers of class <",
    final, "> have been kept (geom_spatraster)"
  )

  return(newr)
}
