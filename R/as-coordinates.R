#' Get cell number, row and column from a `SpatRaster`
#'
#' @description
#'
#' `as_coordinates()` returns the position of each cell in the `SpatRaster`
#' matrix.
#'
#' @seealso [slice.SpatRaster()].
#'
#' @family coerce
#' @param x A `SpatRaster` object.
#' @param as.raster If `TRUE`, the result is a `SpatRaster` object with three
#'   layers indicating the position of each cell (cell number, row and column).
#'
#' @returns
#' A [tibble][tibble::tbl_df] or a `SpatRaster` (if `as.raster = TRUE`) with
#' one row (or cell) for each cell in `x`.
#'
#' When `as.raster = TRUE`, the resulting `SpatRaster` has the same CRS,
#' extent and resolution as `x`.
#'
#' @encoding UTF-8
#' @export
#' @examples
#'
#' library(terra)
#'
#' f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#'
#' r <- rast(f)
#'
#' as_coordinates(r)
#' as_coordinates(r, as.raster = TRUE)
#'
#' as_coordinates(r, as.raster = TRUE) |> plot()
#'
as_coordinates <- function(x, as.raster = FALSE) {
  check_spat_class(x, "SpatRaster")
  check_bool(as.raster)

  # Create cell, row and column indexes.
  df <- data.frame(cellindex = seq_len(terra::ncell(x)))
  rowcol <- as.data.frame(terra::rowColFromCell(x, df$cellindex))
  names(rowcol) <- c("rowindex", "colindex")

  tbl <- dplyr::bind_cols(df, rowcol)
  tbl <- tibble::as_tibble(tbl)

  if (isFALSE(as.raster)) {
    return(tbl)
  }

  # Otherwise, create a raster.

  template <- terra::rast(x)
  terra::crs(template) <- terra::crs(x)
  terra::nlyr(template) <- 3

  terra::values(template) <- unlist(tbl)

  names(template) <- names(tbl)

  template
}
