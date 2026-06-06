#' Drop attributes of `Spat*` objects containing missing values
#'
#' @description
#' - `SpatVector`: `drop_na()` method drops geometries where any attribute
#' specified by `...` contains a missing value.
#' - `SpatRaster`: `drop_na()` method drops cells where any layer specified by
#' `...` contains a missing value.
#'
#' @export
#' @encoding UTF-8
#'
#' @rdname drop_na.Spat
#' @name drop_na.Spat
#'
#' @seealso [tidyr::drop_na()]
#' @family tidyr.missing
#' @family tidyr.methods
#'
#' @importFrom tidyr drop_na
#'
#' @param data A `SpatVector` created with [terra::vect()] or a `SpatRaster`
#'   [terra::rast()].
#' @param ... <[`tidy-select`][tidyr::tidyr_tidy_select]> Attributes to inspect
#'   for missing values. If empty, all attributes are used.
#'
#' @returns A `Spat*` object of the same class as `data`. See **Methods**.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::trim()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::drop_na()] method.
#'
#' ## `SpatVector`
#'
#' The implementation of this method is performed on a `by-attribute` basis,
#' meaning that `NA` values are assessed on the attributes (columns) of each
#' vector (rows). The result is a `SpatVector` with potentially fewer
#' geometries than the input.
#'
#' ## `SpatRaster`
#'
#' `r lifecycle::badge('questioning')`
#'
#' The implementation of `drop_na().SpatRaster` can be understood as a
#' masking method based on the values of the layers (see
#' [terra::mask()]).
#'
#' `SpatRaster` layers are considered as columns and `SpatRaster` cells as rows,
#' so rows (cells) with any `NA` value on any layer become `NA`. You can also
#' mask the cells (rows) based on the values of specific
#' layers (columns).
#'
#' `drop_na()` effectively removes outer cells that are `NA` (see
#' [terra::trim()]), so the extent of the resulting object may differ from the
#' extent of the input (see [terra::resample()] for more information).
#'
#' Check the **Examples** to have a better understanding of this method.
#'
#' ### Feedback needed!
#'
#' Visit <https://github.com/dieghernan/tidyterra/issues>. The implementation
#' of this method for `SpatRaster` may change in the future.
#'
#' @examples
#'
#' library(terra)
#'
#' f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#'
#' v <- terra::vect(f)
#'
#' # Add missing values.
#' v <- v |> mutate(iso2 = ifelse(cpro <= "09", NA, cpro))
#'
#' # Initial plot.
#' plot(v, col = "red")
#'
#' # Drop geometries with missing values in iso2.
#' v |>
#'   drop_na(iso2) |>
#'   plot(col = "red")
drop_na.SpatVector <- function(data, ...) {
  tbl <- as_tbl_internal(data)
  dropped <- tidyr::drop_na(tbl, ...)

  # Use own method, there is no way to avoid coercion.
  if (nrow(dropped) == 0) {
    cli::cli_alert_warning(paste0(
      cli::col_red("All geometries dropped."),
      "\nReturning an empty {.cls SpatVector}."
    ))
    vend <- terra::vect("MULTIPOINT EMPTY")
    terra::crs(vend) <- pull_crs(data)

    return(vend)
  }

  dropped <- restore_attr(dropped, tbl)

  vend <- as_spat_internal(dropped)
  vend <- group_prepare_spat(vend, dropped)

  vend
}

#' @export
#' @encoding UTF-8
#' @rdname drop_na.Spat
#'
#' @examples
#' # SpatRaster method
#'
#' \donttest{
#' r <- rast(
#'   crs = "EPSG:3857",
#'   extent = c(0, 10, 0, 10),
#'   nlyr = 3,
#'   resolution = c(2.5, 2.5)
#' )
#' terra::values(r) <- seq_len(ncell(r) * nlyr(r))
#'
#' # Add missing values.
#' r[r > 13 & r < 22 | r > 31 & r < 45] <- NA
#'
#' # Initial plot.
#' plot(r, nc = 3)
#'
#' # Mask with lyr.1.
#' r |>
#'   drop_na(lyr.1) |>
#'   plot(nc = 3)
#'
#' # Mask with lyr.2.
#' r |>
#'   drop_na(lyr.2) |>
#'   plot(nc = 3)
#'
#' # Mask with lyr.3.
#' r |>
#'   drop_na(lyr.3) |>
#'   plot(nc = 3)
#'
#' # Mask all layers.
#' r |>
#'   drop_na() |>
#'   plot(nc = 3)
#' }
drop_na.SpatRaster <- function(data, ...) {
  # Create a matrix to assess results without converting to a data frame.
  m <- matrix(nrow = terra::nlyr(data), ncol = terra::nlyr(data))
  diag(m) <- seq_len(terra::nlyr(data))

  df <- as.data.frame(m)
  names(df) <- names(data)

  dropped <- tidyr::drop_na(df, ...)

  # Use template to identify operations
  if (nrow(dropped) == 0) {
    # Mask all layers.
    to_mask <- seq_len(terra::nlyr(data))
  } else {
    to_mask <- as.integer(dropped[1, ])
    to_mask <- to_mask[!is.na(to_mask)]
  }

  # Mask each selected layer.
  end <- data
  for (i in to_mask) {
    mask <- terra::subset(data, i)
    end <- terra::mask(end, mask)
  }

  # Trim the extent.
  newrast <- terra::trim(end)

  newrast
}

#' @export
tidyr::drop_na
