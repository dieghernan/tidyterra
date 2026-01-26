#' Subset cells/geometries of `Spat*` objects
#'
#' @description
#' The `filter()` function is used to subset `Spat*` objects, retaining all
#' cells/geometries that satisfy your conditions. To be retained, the
#' cell/geometry must produce a value of `TRUE` for all conditions.
#'
#' **It is possible to filter a `SpatRaster` by its geographic coordinates**.
#' You need to use `filter(.data, x > 42)`. Note that `x` and `y` are reserved
#' names on \CRANpkg{terra}, since they refer to the geographic coordinates of
#' the layer.
#'
#' See **Examples** and section **About layer names** on [as_tibble.Spat()].
#'
#' @export
#' @rdname filter.Spat
#' @name filter.Spat
#'
#' @seealso [dplyr::filter()]
#'
#' @family single table verbs
#' @family dplyr.rows
#' @family dplyr.methods
#'
#' @importFrom dplyr filter
#' @inheritParams select.Spat
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expressions that
#'   return a logical value, and are defined in terms of the layers/attributes
#'   in `.data`. If multiple expressions are included, they are combined with
#'   the `&` operator. Only cells/geometries for which all conditions evaluate
#'   to `TRUE` are kept. See **Methods**.
#' @param .preserve Ignored for `Spat*` objects.
#' @param .keep_extent Should the extent of the resulting `SpatRaster` be kept?
#'   On `FALSE`, [terra::trim()] is called so the extent of the result may be
#'   different of the extent of the output. See also [drop_na.SpatRaster()].
#'
#' @inherit select.Spat return
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::filter()] method.
#'
#' ## `SpatRaster`
#'
#' Cells that do not fulfill the conditions on `...` are returned with value
#' `NA`. On a multi-layer `SpatRaster` the `NA` is propagated across all the
#' layers.
#'
#' If `.keep_extent = TRUE` the returning `SpatRaster` has the same CRS, extent,
#' resolution and hence the same number of cells than `.data`. If
#' `.keep_extent = FALSE` the outer `NA` cells are trimmed with [terra::trim()],
#' so the extent and number of cells may differ. The output would present in
#' any case the same CRS and resolution than `.data`.
#'
#' `x` and `y` variables (i.e. the longitude and latitude of the `SpatRaster`)
#' are also available internally for filtering. See **Examples**.
#'
#' ## `SpatVector`
#'
#' The result is a `SpatVector` with all the geometries that produce a value of
#' `TRUE` for all conditions.
#'
#' @examples
#'
#' library(terra)
#' f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#'
#' r <- rast(f) |> select(tavg_04)
#'
#' plot(r)
#'
#' # Filter temps
#' r_f <- r |> filter(tavg_04 > 11.5)
#'
#' # Extent is kept
#' plot(r_f)
#'
#' # Filter temps and extent
#' r_f2 <- r |> filter(tavg_04 > 11.5, .keep_extent = FALSE)
#'
#' # Extent has changed
#' plot(r_f2)
#'
#' # Filter by geographic coordinates
#' r2 <- project(r, "epsg:4326")
#'
#' r2 |> plot()
#'
#' r2 |>
#'   filter(
#'     x > -4,
#'     x < -2,
#'     y > 42
#'   ) |>
#'   plot()
filter.SpatRaster <- function(
  .data,
  ...,
  .preserve = FALSE,
  .keep_extent = TRUE
) {
  df <- as_tbl_internal(.data)
  xy <- dplyr::select(df, 1:2)
  values <- df

  # Filter
  filtered <- dplyr::filter(values, ...)

  # Rebuild raster
  rebuild_df <- dplyr::left_join(xy, filtered, by = c("x", "y"))

  # For dtplyr
  rebuild_df <- data.table::as.data.table(rebuild_df)
  attributes(rebuild_df) <- attributes(df)

  newrast <- as_spat_internal(rebuild_df)

  if (!isTRUE(.keep_extent)) {
    newrast <- terra::trim(newrast)
  }

  if (any(terra::has.colors(.data))) {
    terra::coltab(newrast) <- terra::coltab(.data)
  }
  newrast
}

#' @export
#' @rdname filter.Spat
filter.SpatVector <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  # Use own method
  tbl <- as_tibble(.data)

  var_index <- make_safe_index("tterra_index", tbl)
  tbl[[var_index]] <- seq_len(nrow(tbl))

  filtered <- dplyr::filter(tbl, ..., .by = {{ .by }}, .preserve = .preserve)

  vend <- .data[as.integer(filtered[[var_index]]), ]

  vend <- group_prepare_spat(vend, filtered)

  vend
}

#' @export
dplyr::filter

# TODO: Implement method when available

filter_out <- filter.SpatVector
