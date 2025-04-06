#' Visualise `SpatRaster` objects
#'
#' @description
#'
#' This geom is used to visualise `SpatRaster` objects (see [terra::rast()]).
#' The geom is designed for visualise the object by layers, as [terra::plot()]
#' does.
#'
#' For plotting `SpatRaster` objects as map tiles (i.e. RGB `SpatRaster`), use
#' [geom_spatraster_rgb()].
#'
#' The underlying implementation is based on [ggplot2::geom_raster()].
#'
#' @return A \CRANpkg{ggplot2} layer
#' @family ggplot2.utils
#' @param data A `SpatRaster` object.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. See
#'   **Aesthetics** specially in the use of `fill` aesthetic.
#'
#' @param na.rm If `TRUE`, the default, missing values are silently removed. If
#'   `FALSE`, missing values are removed with a warning.
#'
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather
#'   than combining with them.
#'
#' @param maxcell positive integer. Maximum number of cells to use for
#'   the plot.
#'
#' @param use_coltab Logical. Only applicable to `SpatRaster` objects that have
#'   an associated [coltab][terra::coltab()]. Should the coltab be used on the
#'   plot? See also [scale_fill_coltab()].
#'
#' @param mask_projection logical, defaults to `FALSE`. If `TRUE`, mask out
#'   areas outside the input extent. For example, to avoid data wrapping
#'   around the date-line in Equal Area projections. This parameter is passed
#'   to [terra::project()] when reprojecting the `SpatRaster`.
#'
#' @inheritParams ggplot2::geom_raster
#'
#' @source
#' Based on the `layer_spatial()` implementation on \CRANpkg{ggspatial} package.
#' Thanks to [Dewey Dunnington](https://github.com/paleolimbot) and [ggspatial
#' contributors](https://github.com/paleolimbot/ggspatial/graphs/contributors).
#'
#' @seealso [ggplot2::geom_raster()], [ggplot2::coord_sf()],
#' [ggplot2::facet_wrap()]
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::plot()]
#'
#' @section Coords:
#'
#' When the `SpatRaster` does not present a crs (i.e.,
#' `terra::crs(rast) == ""`) the geom does not make any assumption on the
#' scales.
#'
#' On `SpatRaster` that have a crs, the geom uses [ggplot2::coord_sf()] to
#' adjust the scales. That means that also the
#' **`SpatRaster` may be reprojected**.
#'
#' @section Aesthetics:
#'
#' `geom_spatraster()` understands the following aesthetics:
#'
#'  - [`fill`][ggplot2::aes_colour_fill_alpha]
#'  - [`alpha`][ggplot2::aes_colour_fill_alpha]
#'
#' If `fill` is not provided, `geom_spatraster()` creates a \CRANpkg{ggplot2}
#' layer with all the layers of the `SpatRaster` object. Use `facet_wrap(~lyr)`
#' to display properly the `SpatRaster` layers.
#'
#' If `fill` is used, it should contain the name of one layer that is present
#' on the `SpatRaster` (i.e.
#' `geom_spatraster(data = rast, aes(fill = <name_of_lyr>)`). Names of the
#' layers can be retrieved using `names(rast)`.
#'
#' Using `geom_spatraster(..., mapping = aes(fill = NULL))` or
#' `geom_spatraster(..., fill = <color value(s)>)` would create a layer with no
#' mapped `fill` aesthetic.
#'
#' `fill` can use computed variables.
#'
#' For `alpha` use computed variable. See section **Computed variables**.
#'
#' @section Facets:
#'
#' You can use ` facet_wrap(~lyr)` for creating a faceted plot by each layer of
#' the `SpatRaster` object. See [ggplot2::facet_wrap()] for details.
#'
#' @section Computed variables:
#'
#' This geom computes internally some variables that are available for use as
#' aesthetics, using (for example) `aes(alpha = after_stat(value))` (see
#' [ggplot2::after_stat()]).
#'
#'  - `after_stat(value)`: Values of the `SpatRaster.`
#'  - `after_stat(lyr)`: Name of the layer.
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
#'   scale_fill_grass_c(palette = "celsius")
#'
#' # Display facets
#' ggplot() +
#'   geom_spatraster(data = temp_rast) +
#'   facet_wrap(~lyr, ncol = 2) +
#'   scale_fill_grass_b(palette = "celsius", breaks = seq(0, 20, 2.5))
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
                            use_coltab = TRUE,
                            mask_projection = FALSE,
                            ...) {
  if (!inherits(data, "SpatRaster")) {
    cli::cli_abort(paste(
      "{.fun tidyterra::geom_spatraster} only works with",
      "{.cls SpatRaster} objects, not {.cls {class(data)}}.",
      "See {.help terra::vect}"
    ))
  }

  # Kindly warn in RGB
  if (terra::has.RGB(data)) {
    cli::cli_alert_warning(paste(
      "RGB specification detected. Maybe use",
      cli::style_bold("{.fun tidyterra::geom_spatraster_rgb}:"),
      "instead?"
    ))
  }

  # 1. Work with aes ----

  dots <- list(...)
  raster_names <- names(data)

  prepared <- prepare_aes_spatraster(mapping, raster_names, dots)

  # Use prepared data
  mapping <- prepared$map

  # Check if need to subset the SpatRaster
  if (is.character(prepared$namelayer)) {
    # Subset the layer from the data
    data <- terra::subset(data, prepared$namelayer)
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

  if (all(use_coltab, any(terra::has.colors(data)))) {
    layer_spatrast <- c(
      layer_spatrast,
      scale_fill_coltab(data = data)
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
    lyr = lyr, group = lyr,
    spatraster = after_stat(spatraster)
  ),
  extra_params = c("maxcell", "na.rm", "coord_crs", "mask_projection"),
  compute_layer = function(self, data, params, layout) {
    # warn if not using facets
    if (length(unique(data$PANEL)) != length(unique(data$lyr))) {
      nly <- length(unique(data$lyr))
      if (nly > 1) {
        cli::cli_alert_warning(paste(
          cli::style_bold("{.fun tidyterra::geom_spatraster}:"),
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
  compute_group = function(data, scales, coord, params,
                           coord_crs = NA, mask_projection = FALSE) {
    # Extract raster from group
    rast <- data$spatraster[[1]]

    # Reproject if needed
    rast <- reproject_raster_on_stat(rast, coord_crs, mask = mask_projection)

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
                                     coords_crs = NA,
                                     mask = FALSE) {
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
        "{.fun geom_spatraster_*} on {.cls SpatRaster}s with crs",
        "must be used with {.fun ggplot2::coord_sf}."
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
    x = init_rast,
    y = coord_crs,
    mask = mask
  )

  # Try to keep the same number of cells on the template
  template <- terra::spatSample(template, terra::ncell(init_rast),
    as.raster = TRUE,
    method = "regular"
  )


  # Reproject
  proj_rast <- terra::project(init_rast, template, mask = mask)

  return(proj_rast)
}

pivot_longer_spat <- function(x) {
  tb <- as_tbl_internal(x)
  tb_pivot <- tidyr::pivot_longer(tb, -c(1, 2),
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
    cli::cli_inform(paste(
      "{.cls SpatRaster} resampled to",
      "{.field {terra::ncell(r)}} cell{?s}."
    ))
  }

  return(r)
}

check_mixed_cols <- function(r, fn = "tidyterra::geom_spat*") {
  todf <- terra::as.data.frame(r[1], xy = FALSE)
  col_classes <- unlist(lapply(todf, class))

  # Double and integer treated both as numeric, no warning
  col_classes <- gsub("integer|numeric|double", "numeric", col_classes)

  # If all the same class then do nothing
  if (length(unique(col_classes)) == 1) {
    # If is factor use combineLevels (terra >= 1.8-10)
    if (col_classes[1] == "factor") {
      rend <- try(terra::combineLevels(r), silent = TRUE)
      if (inherits(rend, "try-error")) {
        return(r)
      } else {
        return(rend)
      }
    }
    return(r)
  }

  # If not, select the first class

  final <- col_classes[1]

  # Work with indexes
  extract_vars <- as.integer(which(col_classes == final))
  newr <- terra::subset(r, extract_vars)

  cli::cli_warn("Mixed layer classes found in {.fun {fn}}.")
  cli::cli_alert_warning(paste(
    "Plotting only{qty(length(extract_vars))}",
    "layer {.val {names(newr)}} of class {.cls {final}}"
  ))

  # If is factor use combineLevels (terra >= 1.8-10)
  if (final == "factor") {
    rend <- try(terra::combineLevels(newr), silent = TRUE)
    if (inherits(rend, "try-error")) {
      return(newr)
    } else {
      return(rend)
    }
  }

  return(newr)
}


prepare_aes_spatraster <- function(mapping = aes(),
                                   raster_names = NA,
                                   dots = list()) {
  # Prepare aes for StatTerraSpatRaster
  mapinit <- cleanup_aesthetics(mapping, "group")

  mapinit <- override_aesthetics(
    mapinit,
    ggplot2::aes(
      spatraster = .data$spatraster,
      # For faceting
      lyr = .data$lyr,
      group = .data$lyr
    )
  )

  # Create first the default result object, would be overriden

  result_obj <- list(
    namelayer = FALSE,
    map = mapinit
  )


  # Capture all info
  fill_from_dots <- "fill" %in% names(dots)

  # Do nothing if fill in dots
  if (isTRUE(fill_from_dots)) {
    return(result_obj)
  }


  # Extract from aes
  fill_from_aes <- unname(vapply(
    mapinit,
    rlang::as_label, character(1)
  )["fill"])
  fill_not_provided <- is.na(fill_from_aes)
  is_layer <- fill_from_aes %in% raster_names

  # If not provided add after_stat
  if (fill_not_provided) {
    map_not_prov <- override_aesthetics(
      mapinit, ggplot2::aes(fill = after_stat(.data$value))
    )

    result_obj$map <- map_not_prov
    return(result_obj)
  }

  # If it is a layer need to override the fill value and keep the namelayer
  if (is_layer) {
    map_layer <- override_aesthetics(
      ggplot2::aes(fill = after_stat(.data$value)),
      mapinit
    )


    result_obj$map <- map_layer
    result_obj$namelayer <- fill_from_aes
    return(result_obj)
  }

  # Otherwise leave as it is
  return(result_obj)
}
