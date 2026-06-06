#' Get cell number, row and column from a `SpatRaster`
#'
#' @description
#'
#' `as_coordinates()` can be used to obtain the position of each cell on the
#' `SpatRaster` matrix.
#'
#' @export
#' @encoding UTF-8
#'
#' @seealso [slice.SpatRaster()]
#'
#' @family coerce
#' @param x A `SpatRaster` object.
#' @param as.raster If `TRUE`, the result is a `SpatRaster` object with three
#'   layers indicating the position of each cell (cell number, row and column).
#'
#' @returns
#' A [tibble][tibble::tbl_df] or a `SpatRaster` (if `as.raster = TRUE`) with
#' the same number of rows (or cells) as the number of cells in `x`.
#'
#' When `as.raster = TRUE` the resulting `SpatRaster` has the same CRS,
#' extent and resolution as `x`.
#'
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
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort(paste(
      "{.fun tidyterra::as_coordinates} requires a {.cls SpatRaster} object,",
      "not {.cls {class(x)}}."
    ))
  }

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
