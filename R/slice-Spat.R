#' Subset cells/rows/columns/geometries using their positions
#'
#' @description
#'
#' `slice()` lets you index cells/rows/columns/geometries by their (integer)
#' locations. It allows you to select, remove, and duplicate those dimensions
#' of a Spat* object.
#'
#' **If you want to slice your SpatRaster by geographic coordinates** use
#' [filter.SpatRaster()] method.
#'
#' It is accompanied by a number of helpers for common use cases:
#'
#' - `slice_head()` and `slice_tail()` select the first or last
#'    cells/geometries.
#'
#' - `slice_sample()` randomly selects cells/geometries.
#'
#' - `slice_rows()` and `slice_cols()` allow to subset entire rows or columns,
#'   of a SpatRaster.
#'
#' - `slice_colrows()` subsets regions of the raster by row and column position
#'   of a SpatRaster.
#'
#' You can get a skeleton of your SpatRaster with the cell, column and row
#' index with [as_coordinates()].
#'
#' See **Methods** for details.
#'
#' @export
#' @rdname slice.Spat
#' @name slice.Spat
#'
#' @seealso
#' [dplyr::slice()], [terra::spatSample()].
#'
#' You can get a skeleton of your SpatRaster with the cell, column and row
#' index with [as_coordinates()].
#'
#' If you want to slice by geographic coordinates use [filter.SpatRaster()].
#'
#' @family single table verbs
#' @family dplyr.rows
#' @family dplyr.methods
#'
#' @return A Spat* object  of the same class than `.data`. See **Methods**.
#'
#' @importFrom dplyr slice
#'
#' @inheritParams mutate.Spat
#'
#' @param .preserve Ignored for Spat* objects
#' @param .keep_extent Should the extent of the resulting SpatRaster be kept?
#'   See also [terra::trim()], [terra::extend()].
#' @param ... [`data-masking`][dplyr::slice] Integer row values. Provide
#'   either positive values to keep, or negative values to drop.
#'
#'   The values provided must be either all positive or all negative. Indices
#'   beyond the number of rows in the input are silently ignored.
#'   See **Methods**.
#' @param cols,rows Integer col/row values of the SpatRaster
#' @param inverse If `TRUE`, `.data` is inverse-masked to the given selection.
#'   See [terra::mask()].
#' @param na.rm Logical, should cells that present a value of `NA` removed when
#'   computing `slice_min()/slice_max()`?. The default is `TRUE`.
#' @inheritParams dplyr::slice
#'
#' @section terra equivalent:
#'
#' [terra::subset()], [terra::spatSample()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::slice()] function.
#'
#' ## SpatRaster
#'
#' The result is a SpatRaster with the crs and resolution of the input and
#' where cell values of the selected cells/columns/rows are preserved.
#'
#' Use `.keep_extent = TRUE` to preserve the extent of `.data` on the output.
#' The non-selected cells would present a value of `NA`.
#'
#' ## SpatVector
#'
#' This method relies on the implementation of [dplyr::slice()] method on the
#' sf package. The result is a SpatVector where the attributes of the selected
#' geometries are preserved.
#'
#' @examples
#'
#'
#' library(terra)
#'
#' f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#' r <- rast(f)
#'
#' # Slice first 100 cells
#' r %>%
#'   slice(1:100) %>%
#'   plot()
#'
#' # Rows
#' r %>%
#'   slice_rows(1:30) %>%
#'   plot()
#'
#' # Cols
#' r %>%
#'   slice_cols(-(20:50)) %>%
#'   plot()
#'
#' # Spatial sample
#' r %>%
#'   slice_sample(prop = 0.2) %>%
#'   plot()
#'
#'
#' # Slice regions
#' r %>%
#'   slice_colrows(
#'     cols = c(20:40, 60:80),
#'     rows = -c(1:20, 30:50)
#'   ) %>%
#'   plot()
slice.SpatRaster <- function(.data, ..., .preserve = FALSE,
                             .keep_extent = FALSE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)


  sliced <- dplyr::slice(skeleton, ...)

  keepcells <- sliced$cellindex

  # Make NA cells

  # To NA

  tonas <- setdiff(skeleton$cellindex, keepcells)

  newrast <- .data
  newrast[tonas] <- NA

  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }


  # Crop to selected range
  range <- range(keepcells)
  keepindex <- seq(range[1], range[2], by = 1)
  newrast <- newrast[keepindex, drop = FALSE]


  return(newrast)
}
#' @export
#' @rdname slice.Spat
slice.SpatVector <- function(.data, ..., .preserve = FALSE) {
  # Use sf method
  sf_obj <- sf::st_as_sf(.data)
  sliced <- dplyr::slice(sf_obj, ..., .preserve = .preserve)

  return(terra::vect(sliced))
}
#' @export
#' @rdname slice.Spat
#' @importFrom dplyr slice_head
slice_head.SpatRaster <- function(.data, ..., n, prop, .keep_extent = FALSE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)

  sliced <- dplyr::slice_head(skeleton, ..., n = n, prop = prop)

  keepcells <- sliced$cellindex


  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex, keepcells)

  newrast <- .data
  newrast[tonas] <- NA

  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  newrast <- newrast[keepcells, drop = FALSE]

  return(newrast)
}

#' @export
#' @rdname slice.Spat
slice_head.SpatVector <- function(.data, ..., n, prop) {
  # Use sf method
  sf_obj <- sf::st_as_sf(.data)
  sliced <- dplyr::slice_head(sf_obj, ..., n = n, prop = prop)

  return(terra::vect(sliced))
}

#' @export
#' @rdname slice.Spat
#' @importFrom dplyr slice_tail
slice_tail.SpatRaster <- function(.data, ..., n, prop, .keep_extent = FALSE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)

  sliced <- dplyr::slice_tail(skeleton, ..., n = n, prop = prop)

  keepcells <- sliced$cellindex

  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex, keepcells)

  newrast <- .data
  newrast[tonas] <- NA

  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  newrast <- newrast[keepcells, drop = FALSE]

  return(newrast)
}

#' @export
#' @rdname slice.Spat
slice_tail.SpatVector <- function(.data, ..., n, prop) {
  # Use sf method
  sf_obj <- sf::st_as_sf(.data)
  sliced <- dplyr::slice_tail(sf_obj, ..., n = n, prop = prop)

  return(terra::vect(sliced))
}

#' @export
#' @rdname slice.Spat
#' @importFrom dplyr slice_min
slice_min.SpatRaster <- function(.data, order_by, ..., n, prop,
                                 with_ties = TRUE, .keep_extent = FALSE,
                                 na.rm = TRUE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)
  values <- as_tibble(.data, na.rm = FALSE, xy = FALSE)

  # Fix names just in case
  names(skeleton) <- paste0(names(skeleton), ".tidyterra")

  # Add values
  skeleton <- dplyr::bind_cols(skeleton, values)

  # Remove NAs
  if (na.rm) skeleton <- tidyr::drop_na(skeleton)

  sliced <- dplyr::slice_min(skeleton,
    order_by = {{ order_by }},
    ..., n = n, prop = prop,
    with_ties = with_ties
  )

  keepcells <- sliced$cellindex.tidyterra

  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex.tidyterra, keepcells)

  newrast <- .data
  newrast[tonas] <- NA


  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  # Crop to selected range
  range <- range(keepcells)
  keepindex <- seq(range[1], range[2], by = 1)
  newrast <- newrast[keepindex, drop = FALSE]

  return(newrast)
}

#' @export
#' @rdname slice.Spat
slice_min.SpatVector <- function(.data, order_by, ..., n, prop,
                                 with_ties = TRUE) {
  # Use sf method
  sf_obj <- sf::st_as_sf(.data)
  sliced <- dplyr::slice_min(sf_obj, ...,
    order_by = {{ order_by }},
    ..., n = n, prop = prop,
    with_ties = with_ties
  )

  return(terra::vect(sliced))
}

#' @export
#' @rdname slice.Spat
#' @importFrom dplyr slice_max
slice_max.SpatRaster <- function(.data, order_by, ..., n, prop,
                                 with_ties = TRUE, .keep_extent = FALSE,
                                 na.rm = TRUE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)
  values <- as_tibble(.data, na.rm = FALSE, xy = FALSE)

  # Fix names just in case
  names(skeleton) <- paste0(names(skeleton), ".tidyterra")

  # Add values
  skeleton <- dplyr::bind_cols(skeleton, values)

  # Remove NAs
  if (na.rm) skeleton <- tidyr::drop_na(skeleton)

  sliced <- dplyr::slice_max(skeleton,
    order_by = {{ order_by }},
    ..., n = n, prop = prop,
    with_ties = with_ties
  )

  keepcells <- sliced$cellindex.tidyterra

  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex.tidyterra, keepcells)

  newrast <- .data
  newrast[tonas] <- NA


  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  # Crop to selected range
  range <- range(keepcells)
  keepindex <- seq(range[1], range[2], by = 1)
  newrast <- newrast[keepindex, drop = FALSE]

  return(newrast)
}

#' @export
#' @rdname slice.Spat
slice_max.SpatVector <- function(.data, order_by, ..., n, prop,
                                 with_ties = TRUE) {
  # Use sf method
  sf_obj <- sf::st_as_sf(.data)
  sliced <- dplyr::slice_max(sf_obj, ...,
    order_by = {{ order_by }},
    ..., n = n, prop = prop,
    with_ties = with_ties
  )

  return(terra::vect(sliced))
}

#' @export
#' @rdname slice.Spat
#' @importFrom dplyr slice_sample
slice_sample.SpatRaster <- function(.data, ..., n, prop,
                                    weight_by = NULL, replace = FALSE,
                                    .keep_extent = FALSE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)
  values <- as_tibble(.data, na.rm = FALSE, xy = FALSE)

  # Fix names just in case
  names(skeleton) <- paste0(names(skeleton), ".tidyterra")

  # Add values
  skeleton <- dplyr::bind_cols(skeleton, values)

  sliced <- dplyr::slice_sample(skeleton, ...,
    n = n,
    prop = prop, weight_by = weight_by,
    replace = replace
  )

  keepcells <- sliced$cellindex.tidyterra

  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex.tidyterra, keepcells)

  newrast <- .data
  newrast[tonas] <- NA

  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  # Crop to selected range
  range <- range(keepcells)
  keepindex <- seq(range[1], range[2], by = 1)
  newrast <- newrast[keepindex, drop = FALSE]

  return(newrast)
}

#' @export
#' @rdname slice.Spat
slice_sample.SpatVector <- function(.data, ..., n, prop,
                                    weight_by = NULL, replace = FALSE) {
  # Use sf method
  sf_obj <- sf::st_as_sf(.data)
  sliced <- dplyr::slice_sample(sf_obj, ...,
    n = n, prop = prop,
    replace = replace
  )

  return(terra::vect(sliced))
}
#' @export
#' @rdname slice.Spat
slice_rows <- function(.data, ...) {
  UseMethod("slice_rows")
}

#' @export
#' @rdname slice.Spat
slice_rows.SpatRaster <- function(.data, ..., .keep_extent = FALSE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)

  index <- skeleton["rowindex"]
  index$rowindex <- sort(index$rowindex)
  index <- dplyr::distinct(index)

  slice_dim <- dplyr::slice(index, ...)

  # Get cells to make NA
  sliced <- dplyr::inner_join(skeleton,
    slice_dim,
    by = "rowindex"
  )

  keepcells <- sliced$cellindex

  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex, keepcells)

  newrast <- .data
  newrast[tonas] <- NA

  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  # Crop to selected range
  range <- range(slice_dim$rowindex)
  keepindex <- seq(range[1], range[2], by = 1)
  newrast <- newrast[keepindex, , drop = FALSE]

  return(newrast)
}

#' @export
#' @rdname slice.Spat
slice_cols <- function(.data, ...) {
  UseMethod("slice_cols")
}

#' @export
#' @rdname slice.Spat
slice_cols.SpatRaster <- function(.data, ..., .keep_extent = FALSE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)

  index <- skeleton["colindex"]
  index$colindex <- sort(index$colindex)
  index <- dplyr::distinct(index)

  slice_dim <- dplyr::slice(index, ...)

  # Get cells to make NA
  sliced <- dplyr::inner_join(skeleton,
    slice_dim,
    by = "colindex"
  )

  keepcells <- sliced$cellindex

  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex, keepcells)

  newrast <- .data
  newrast[tonas] <- NA

  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  # Crop to selected range
  range <- range(slice_dim$colindex)
  keepindex <- seq(range[1], range[2], by = 1)
  newrast <- newrast[, keepindex, drop = FALSE]

  return(newrast)
}

#' @export
#' @rdname slice.Spat
slice_colrows <- function(.data, ...) {
  UseMethod("slice_colrows")
}

#' @export
#' @rdname slice.Spat
slice_colrows.SpatRaster <- function(.data, ..., cols, rows,
                                     .keep_extent = FALSE,
                                     inverse = FALSE) {
  # Create skeleton
  skeleton <- as_coordinates(.data)

  index <- skeleton["colindex"]
  index$colindex <- sort(index$colindex)
  index <- dplyr::distinct(index)

  # Cols
  col_index <- skeleton["colindex"]
  col_index$colindex <- sort(col_index$colindex)
  col_index <- dplyr::distinct(col_index)

  slice_cols <- dplyr::slice(col_index, cols)


  # Rows
  row_index <- skeleton["rowindex"]
  row_index$rowindex <- sort(row_index$rowindex)
  row_index <- dplyr::distinct(row_index)

  slice_rows <- dplyr::slice(row_index, rows)

  # Get cells to make NA
  sliced <- dplyr::inner_join(skeleton,
    slice_cols,
    by = "colindex"
  )

  sliced <- dplyr::inner_join(sliced,
    slice_rows,
    by = "rowindex"
  )


  keepcells <- sliced$cellindex

  # Make NA cells

  # To NA
  tonas <- setdiff(skeleton$cellindex, keepcells)

  newrast <- .data
  newrast[tonas] <- NA

  # With keep_extent we just replaced the cells with NAs
  if (.keep_extent) {
    return(newrast)
  }

  # Crop to selected range
  # cols
  range_col <- range(sliced$colindex)
  keepindex_col <- seq(range_col[1], range_col[2], by = 1)

  range_row <- range(sliced$rowindex)
  keepindex_row <- seq(range_row[1], range_row[2], by = 1)

  newrast <- newrast[keepindex_row, keepindex_col, drop = FALSE]


  return(newrast)
}

#' @export
dplyr::slice

#' @export
dplyr::slice_head

#' @export
dplyr::slice_max

#' @export
dplyr::slice_min

#' @export
dplyr::slice_tail

#' @export
dplyr::slice_sample
