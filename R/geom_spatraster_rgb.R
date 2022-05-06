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
#' [ggspatial contributors](https://github.com/paleolimbot/ggspatial/graphs/contributors).
#'
#'
#' @inheritParams geom_spatraster
#' @inheritParams scale_fill_terrain
#' @param mapping Ignored.
#' @param r,g,b Integer representing the number of layer of `data` to be
#'  considered as the red (`r`), green (`g`) and blue (`b`) channel.
#'
#' @seealso [ggplot2::geom_raster()], [ggplot2::coord_sf()]. You can get
#' also RGB tiles from the {maptiles} package, see [maptiles::get_tiles()].
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
                                interpolate = FALSE,
                                r = 1,
                                g = 2,
                                b = 3,
                                alpha = 0.9,
                                maxcell = 500000,
                                ...) {
  if (!inherits(data, "SpatRaster")) {
    stop(
      "geom_spatraster() only works with SpatRaster objects. ",
      "See ?terra::vect"
    )
  }

  nlyrs_data <- seq_len(terra::nlyr(data))

  if (!all(
    r %in% nlyrs_data,
    g %in% nlyrs_data,
    b %in% nlyrs_data
  )) {
    cli::cli_abort(paste(
      "Incorrect number of layers on r,g,b. data has",
      nlyrs_data, "layer(s)."
    ))
  }


  # 1. Work with aes ----
  mapping <- override_aesthetics(
    mapping,
    ggplot2::aes_string(
      spatraster = "spatraster"
    )
  )



  # Select channels
  data <- terra::subset(data, c(r, g, b))
  names(data) <- c("r", "g", "b")



  # 2. Check if resample is needed----
  data <- resample_spat(data, maxcell)

  # 3. Build layer ----
  crs_terra <- pull_crs(data)

  if (is.na(crs_terra)) {
    # SpatRaster with no crs

    ggplot2::layer(
      data = tibble::tibble(
        spatraster = list(data)
      ),
      mapping = mapping,
      stat = StatTerraSpatRasterRGBDF,
      geom = GeomTerraSpatRasterRGB,
      position = "identity",
      inherit.aes = FALSE,
      show.legend = FALSE,
      params = list(
        na.rm = TRUE,
        interpolate = interpolate,
        alpha = alpha,
        ...
      )
    )
  } else {
    c(
      ggplot2::layer(
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
          crs_terra = crs_terra,
          maxcell = maxcell,
          interpolate = interpolate,
          alpha = alpha,
          ...
        )
      ),
      # From ggspatial
      # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
      # using the first layer's CRS as the base CRS for coord_sf().
      ggplot2::geom_sf(
        data = sf::st_sfc(sf::st_point(),
          crs = sf::st_crs(data)
        ),
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    )
  }
}

# Stats----

StatTerraSpatRasterRGBDF <- ggplot2::ggproto(
  "StatTerraSpatRasterRGBDF",
  ggplot2::Stat,
  required_aes = c("spatraster", "hexcol"),
  compute_layer = function(self, data, params, layout) {

    # raster extents: ggspatial
    extents <- lapply(data$spatraster, terra::ext)

    extent <- lapply(seq_along(extents), function(i) {
      x <- as.vector(extents[[i]])
    })

    # this needs to be directly in the data so that the position scales
    # get trained
    data$xmin <- vapply(extent, function(x) x["xmin"], numeric(1))
    data$xmax <- vapply(extent, function(x) x["xmax"], numeric(1))
    data$ymin <- vapply(extent, function(x) x["ymin"], numeric(1))
    data$ymax <- vapply(extent, function(x) x["ymax"], numeric(1))

    # Build data
    data$spatraster <- lapply(data$spatraster, make_hexcol)

    # From ggspatial
    data <- tidyr::unnest(data, .data$spatraster)

    data
  }
)

StatTerraSpatRasterRGB <- ggplot2::ggproto(
  "StatTerraSpatRasterRGB",
  ggplot2::Stat,
  required_aes = c("spatraster", "hexcol"),
  extra_params = c("crs_terra", "maxcell"),
  compute_layer = function(self, data, params, layout) {

    # Inspired from ggspatial
    # Make extent and project
    # Check if need to reproject
    coord_crs <- layout$coord_params$crs
    crs_terra <- params$crs_terra

    if (is.null(coord_crs)) {
      cli::cli_abort(
        paste(
          "geom_spatraster_rgb() with georeferend SpatRasters",
          "must be used with coord_sf()."
        ),
        call. = TRUE
      )
    }

    if (!all(!is.null(crs_terra) & !is.null(coord_crs) &
      crs_terra == coord_crs)) {
      init_rast <- data$spatraster[[1]]

      # Create template for projection
      template <- terra::project(
        terra::rast(init_rast),
        pull_crs(coord_crs)
      )

      # Try to keep the same number of cells
      template <- terra::spatSample(template, terra::ncell(init_rast),
        as.raster = TRUE,
        method = "regular"
      )

      # Reproject
      proj_rast <- terra::project(init_rast, template)

      # Nest and build

      data$spatraster <- list(proj_rast)
    }

    # raster extents: ggspatial
    extents <- lapply(data$spatraster, terra::ext)

    extent <- lapply(seq_along(extents), function(i) {
      x <- as.vector(extents[[i]])
    })

    # this needs to be directly in the data so that the position scales
    # get trained
    data$xmin <- vapply(extent, function(x) x["xmin"], numeric(1))
    data$xmax <- vapply(extent, function(x) x["xmax"], numeric(1))
    data$ymin <- vapply(extent, function(x) x["ymin"], numeric(1))
    data$ymax <- vapply(extent, function(x) x["ymax"], numeric(1))

    # Build data
    data$spatraster <- lapply(data$spatraster, make_hexcol)

    # From ggspatial
    data <- tidyr::unnest(data, .data$spatraster)

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

    raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(
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
make_hexcol <- function(data) {
  todf <- as_tbl_spat_attr(data)
  todf <- todf[, 1:5]
  names(todf) <- c("x", "y", "r", "g", "b")

  todf$index <- seq_len(nrow(todf))


  xy <- todf[c("x", "y", "index")]

  values <- todf[, c("index", "r", "g", "b")]

  # Drop nas with index
  full <- tidyr::drop_na(values)
  keepind <- full$index

  # Clamp and make RGB colors
  full[full > 256] <- 256
  full[full < 0] <- 0
  full$index <- keepind

  full$hexcol <- rgb(full$r, full$g, full$b, maxColorValue = 256)


  # Prepare output
  df <- dplyr::left_join(xy, full[c("hexcol", "index")], by = "index")
  return(df[c("x", "y", "hexcol")])
}
