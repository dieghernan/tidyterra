#' Drop cells and attributes of  Spat* objects containing missing values
#'
#' @description
#' `r lifecycle::badge('questioning')`
#'
#' `drop_na()` method drops cells and attributes where any layer or column
#' specified by ... contains a missing value.
#'
#'
#' @return A Spat* object  of the same class than `.data`. See **Methods**.
#'
#' @param data A SpatRaster created with [terra::rast()] or a SpatVector
#'   created with [terra::vect()].
#' @param ... [`tidy-select`][tidyr::drop_na()]  Columns or layers to inspect
#'   for missing values. If empty, all columns/layers are used.
#'
#' @export
#'
#' @rdname drop_na
#' @name drop_na
#'
#' @importFrom tidyr drop_na
#'
#' @seealso [tidyr::drop_na()]
#'
#' @family tidyr.methods
#'
#' @section Feedback needed!:
#'
#' Visit <https://github.com/dieghernan/tidyterra/issues>. The implementation
#' of this method for SpatRaster may change in the future.
#'
#' @section  terra equivalent:
#'
#' [terra::trim()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::drop_na()] function.
#'
#' ## SpatRaster
#'
#' `r lifecycle::badge('questioning')`
#'
#' Actual implementation of `drop_na().SpatRaster` can be understood as a
#' masking method based on the values of the layers (see [terra::mask()]).
#'
#' Raster layers are considered as columns and raster cells as rows, so rows
#' (cells) with any `NA` value on any layer would get a `NA` value. It is
#' possible also to mask the cells (rows) based on the values of specific
#' layers (columns).
#'
#' `drop_na()` would effectively remove outer cells that are `NA` (see
#' [terra::trim()]), so the extent of the resulting object may differ of the
#' extent of the input (see [terra::resample()] for more info).
#'
#' Check the **Examples** to have a better understanding of this method.
#'
#' ## SpatVector
#'
#' The implementation of this method is performed on a `by-attribute` basis,
#' meaning that `NAs` are assessed on the attributes (columns) of each vector
#' (rows). The result is a SpatVector with potentially less geometries than the
#' input
#'
#' @examples
#'
#' library(terra)
#'
#'
#' r <- rast(
#'   crs = "epsg:3857",
#'   extent = c(0, 10, 0, 10),
#'   nlyr = 3,
#'   resolution = c(2.5, 2.5)
#' )
#' terra::values(r) <- seq_len(ncell(r) * nlyr(r))
#'
#'
#'
#' # Add NAs
#' r[r > 13 & r < 22 | r > 31 & r < 45] <- NA
#'
#' # Init
#' plot(r, nc = 3)
#'
#' # Mask with lyr.1
#' r %>%
#'   drop_na(lyr.1) %>%
#'   plot(nc = 3)
#'
#' # Mask with lyr.2
#' r %>%
#'   drop_na(lyr.2) %>%
#'   plot(nc = 3)
#'
#' # Mask with lyr.3
#' r %>%
#'   drop_na(lyr.3) %>%
#'   plot(nc = 3)
#'
#' # Auto-mask all layers
#' r %>%
#'   drop_na() %>%
#'   plot(nc = 3)
#'
drop_na.SpatRaster <- function(data, ...) {
  # Don't need to convert to data.frame
  # Create a matrix to assess results
  m <- matrix(nrow = terra::nlyr(data), ncol = terra::nlyr(data))
  diag(m) <- seq_len(terra::nlyr(data))

  df <- as.data.frame(m)
  names(df) <- names(data)

  dropped <- tidyr::drop_na(df, ...)

  # Use template to identify operations
  if (nrow(dropped) == 0) {
    # All dropped
    to_mask <- seq_len(terra::nlyr(data))
  } else {
    to_mask <- as.integer(dropped[1, ])
    to_mask <- to_mask[!is.na(to_mask)]
  }

  # Subset with a loop
  end <- data
  for (i in to_mask) {
    mask <- terra::subset(data, i)
    end <- terra::mask(end, mask)
  }

  # Trim extent
  newrast <- terra::trim(end)

  return(newrast)
}

#' @export
#' @rdname drop_na
drop_na.SpatVector <- function(data, ...) {

  # Use sf method
  sf_obj <- sf::st_as_sf(data)
  dropped <- tidyr::drop_na(sf_obj, ...)

  return(terra::vect(dropped))
}
