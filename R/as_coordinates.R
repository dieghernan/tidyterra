#' Get cell number, row and column from a SpatRaster
#'
#' @description
#'
#' `as_coordinates()` can be used to obtain the position of each cell on the
#' SpatRaster matrix.
#'
#' @param x A SpatRaster object
#' @param as.raster If `TRUE`, the result is a SpatRaster object with three
#'   layers indicating the position of each cell (cell number, row and column).
#'
#' @return
#' A tibble or a SpatRaster (if `as.raster = TRUE`) with the same number of
#' rows (or cells) than the number of cells in `x`.
#'
#' When `as.raster = TRUE` the resulting SpatRaster has the same crs, extension
#' and resolution than `x`
#'
#' @family coerce
#' @export
#'
#' @seealso [slice.SpatRaster()]
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
#' as_coordinates(r, as.raster = TRUE) %>% plot()
#'
as_coordinates <- function(x, as.raster = FALSE) {
  if (!inherits(x, "SpatRaster")) {
    stop("as_coordinates() needs a SpatRaster object")
  }

  # Create skeleton
  df <- data.frame(cellindex = seq_len(terra::ncell(x)))
  rowcol <- as.data.frame(terra::rowColFromCell(x, df$cellindex))
  names(rowcol) <- c("rowindex", "colindex")

  tbl <- dplyr::bind_cols(df, rowcol)
  tbl <- tibble::as_tibble(tbl)

  if (isFALSE(as.raster)) {
    return(tbl)
  }

  # If not create a raster

  template <- terra::rast(x)
  terra::crs(template) <- terra::crs(x)
  terra::nlyr(template) <- 3

  terra::values(template) <- unlist(tbl)

  names(template) <- names(tbl)

  return(template)
}
