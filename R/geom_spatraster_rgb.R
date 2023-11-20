#' Visualise SpatRaster objects as images
#'
#' @description
#'
#' This geom is used to visualise SpatRaster objects (see [terra::rast()]) as
#' RGB images. The layers are combined such that they represent the red,
#' green and blue channel.
#'
#' For plotting SpatRaster objects by layer values use [geom_spatraster()].
#'
#' The underlying implementation is based on [ggplot2::geom_raster()].
#'
#'
#' @return A ggplot2 layer
#' @family ggplot2.utils
#' @param data A SpatRaster object.
#'
#' @source Based on the `layer_spatial()` implementation on ggspatial package.
#' Thanks to [Dewey Dunnington](https://github.com/paleolimbot) and
#' [ggspatial
#' contributors](https://github.com/paleolimbot/ggspatial/graphs/contributors).
#'
#'
#' @inheritParams geom_spatraster
#' @inheritParams scale_terrain
#' @param mapping Ignored.
#' @param r,g,b Integer representing the number of layer of `data` to be
#'  considered as the red (`r`), green (`g`) and blue (`b`) channel.
#' @param max_col_value Number giving the maximum of the color values range.
#'   When this is `255` (the default), the result is computed most efficiently.
#'   See [grDevices::rgb()].
#'
#' @seealso [ggplot2::geom_raster()], [ggplot2::coord_sf()], [grDevices::rgb()].
#'  You can get also RGB tiles from the \CRANpkg{maptiles} package,
#'  see [maptiles::get_tiles()].
#'
#' @section  terra equivalent:
#'
#' [terra::plotRGB()]
#'
#' @section Coords:
#'
#' When the SpatRaster does not present a crs (i.e.,
#' `terra::crs(rast) == ""`) the geom does not make any assumption on the
#' scales.
#'
#' On SpatRaster that have a crs, the geom uses [ggplot2::coord_sf()] to adjust
#' the scales. That means that also the SpatRaster may be reprojected.
#'
#'
#' @section Aesthetics:
#'
#'
#' No `aes()` is required. In fact, `aes()` will be ignored.
#'
#'
#'
#' @export
#' @examples
#' \donttest{
#'
#' # Tile of Castille and Leon (Spain) from OpenStreetMap
#' file_path <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
#'
#' library(terra)
#' tile <- rast(file_path)
#'
#' library(ggplot2)
#'
#'
#' ggplot() +
#'   geom_spatraster_rgb(data = tile) +
#'   # You can use coord_sf
#'   coord_sf(crs = 3035)
#'
#' # Combine with sf objects
#' vect_path <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#'
#' cyl_sf <- sf::st_read(vect_path)
#'
#' ggplot(cyl_sf) +
#'   geom_spatraster_rgb(data = tile) +
#'   geom_sf(aes(fill = iso2)) +
#'   coord_sf(crs = 3857) +
#'   scale_fill_viridis_d(alpha = 0.7)
#' }
geom_spatraster_rgb <- function(mapping = aes(),
                                data,
                                interpolate = TRUE,
                                r = 1,
                                g = 2,
                                b = 3,
                                alpha = 1,
                                maxcell = 500000,
                                max_col_value = 255,
                                ...) {
  if (!inherits(data, "SpatRaster")) {
    cli::cli_abort(paste(
      "{.fun tidyterra::geom_spatraster_rgb} only works with",
      "{.cls SpatRaster} objects, not {.cls {class(data)}}.",
      "See {.help terra::vect}"
    ))
  }

  layers_order <- as.integer(c(r, g, b))


  nlyrs_data <- seq_len(terra::nlyr(data))

  if (!all(
    layers_order[1] %in% nlyrs_data,
    layers_order[2] %in% nlyrs_data,
    layers_order[3] %in% nlyrs_data
  )) {
    cli::cli_abort(paste(
      "Incorrect number of layers on {.arg {c('r','g','b')}}. data has",
      "{terra::nlyr(data)}", "layer{?s}."
    ))
  }

  # 1. Work with aes ----
  mapping <- override_aesthetics(
    mapping,
    ggplot2::aes(
      spatraster = .data$spatraster
    )
  )

  # Select channels
  data <- terra::subset(data, layers_order)
  names(data) <- c("r", "g", "b")

  # Remove RGB settings, better plot without it
  terra::RGB(data) <- NULL

  # 2. Check if resample is needed----
  data <- resample_spat(data, maxcell)

  # 3. Build layer ----
  crs_terra <- pull_crs(data)


  layer_spatrast <- ggplot2::layer(
    data = tibble::tibble(
      spatraster = list(data)
    ),
    mapping = mapping,
    stat = StatTerraSpatRasterRGB,
    geom = GeomTerraSpatRasterRGB,
    position = "identity",
    inherit.aes = FALSE,
    show.legend = FALSE,
    params = list(
      na.rm = TRUE,
      # Extra params
      maxcell = maxcell,
      interpolate = interpolate,
      max_col_value = max_col_value,
      alpha = alpha,
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

# Stats----
StatTerraSpatRasterRGB <- ggplot2::ggproto(
  "StatTerraSpatRasterRGB",
  ggplot2::Stat,
  required_aes = "spatraster",
  extra_params = c("maxcell", "max_col_value", "na.rm"),
  compute_layer = function(self, data, params, layout) {
    # add coord to the params, so it can be forwarded to compute_group()
    params$coord_crs <- pull_crs(layout$coord_params$crs)

    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(
      data,
      params, layout
    )
  },
  compute_group = function(data, scales, coord, params,
                           coord_crs = NA,
                           max_col_value = 255) {
    # Extract raster from group
    rast <- data$spatraster[[1]]

    # Reproject if needed
    rast <- reproject_raster_on_stat(rast, coord_crs)

    # To data and prepare
    data_end <- make_hexcol(rast, max_col_value)
    data_rest <- data

    # Add data
    data_end$a <- 1
    data_rest$a <- 1

    data <- dplyr::left_join(data_end, data_rest, by = "a")

    data <- remove_columns(data, c("a", "spatraster"))
    data$spatraster <- NA
    data
  }
)


# Geom----

# Based on geom_raster ggplot2
GeomTerraSpatRasterRGB <- ggplot2::ggproto(
  "GeomTerraSpatRasterRGB",
  ggplot2::GeomRaster,
  default_aes = aes(alpha = NA),
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  required_aes = c("x", "y", "hexcol"),
  draw_panel = function(data, panel_params, coord, interpolate = FALSE) {
    data <- coord$transform(data, panel_params)

    # Convert vector of data to raster
    x_pos <- as.integer((data$x - min(data$x)) / ggplot2::resolution(
      data$x,
      FALSE
    ))
    y_pos <- as.integer((data$y - min(data$y)) / ggplot2::resolution(
      data$y,
      FALSE
    ))

    nrow <- max(y_pos) + 1
    ncol <- max(x_pos) + 1

    raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)

    # Setup hexcol from data$hexcol

    raster[cbind(nrow - y_pos, x_pos + 1)] <- ggplot2::alpha(
      data$hexcol,
      data$alpha
    )

    # Figure out dimensions of raster on plot
    x_rng <- c(min(data$xmin, na.rm = TRUE), max(data$xmax, na.rm = TRUE))
    y_rng <- c(min(data$ymin, na.rm = TRUE), max(data$ymax, na.rm = TRUE))


    grid::rasterGrob(raster,
      x = mean(x_rng), y = mean(y_rng),
      width = diff(x_rng), height = diff(y_rng),
      default.units = "native",
      interpolate = interpolate
    )
  }
)


# Helper ------

# Create a table with the hex color of each row (hexcol)
# On any NA then hexcol is returned as NA
make_hexcol <- function(data, max_col_value = 255) {
  # Clamp values
  data[data > max_col_value] <- max_col_value
  data[data < 0] <- 0

  todf <- as_tibble(data, xy = TRUE, na.rm = FALSE)[, 1:5]
  names(todf) <- c("x", "y", "r", "g", "b")

  todf$index <- seq_len(nrow(todf))

  # Split dataset for making color table
  xy <- todf[c("x", "y", "index")]
  values <- todf[c("index", "r", "g", "b")]

  # Drop nas on color table
  full <- tidyr::drop_na(values)
  full$hexcol <- rgb(full$r, full$g, full$b, maxColorValue = max_col_value)


  # Prepare output
  df <- dplyr::left_join(xy, full[c("hexcol", "index")], by = "index")
  return(df[c("x", "y", "hexcol")])
}
