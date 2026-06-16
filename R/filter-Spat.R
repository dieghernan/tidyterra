#' Subset cells/geometries of `Spat*` objects
#'
#' @description
#' These functions subset a data frame by applying the expressions in `...`
#' to determine which rows should be kept (for `filter()`) or dropped (for
#' `filter_out()`).
#'
#' Multiple conditions can be supplied separated by a comma. These will be
#' combined with the `&` operator. To combine comma separated conditions using
#' `|` instead, wrap them in [dplyr::when_any()].
#'
#' Both `filter()` and `filter_out()` treat `NA` like `FALSE`. This subtle
#' behavior can affect how you write your conditions when missing values are
#' involved. See [dplyr::filter()].
#'
#' **You can filter a `SpatRaster` by its geographic coordinates**.
#' Use `filter(.data, x > 42)`. The names `x` and `y` are reserved in
#' \CRANpkg{terra} because they refer to the geographic coordinates of the
#' layer.
#'
#' See **Examples** and section **About layer names** on [as_tibble.Spat()].
#'
#' @export
#' @encoding UTF-8
#' @rdname filter.Spat
#' @name filter.Spat
#'
#' @seealso [dplyr::filter()]
#'
#' @family dplyr.single_table
#' @family dplyr.rows
#' @family dplyr.methods
#'
#' @importFrom dplyr filter
#' @inherit select.Spat return
#'
#' @inheritParams select.Spat
#' @inheritParams dplyr::filter
#'
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expressions that
#'   return a logical value and are defined in terms of the layers/attributes
#'   in `.data`. If multiple expressions are included, they are combined with
#'   the `&` operator. Only cells/geometries for which all conditions evaluate
#'   to `TRUE` are kept. See **Methods**.
#' @param .keep_extent Logical. If `TRUE`, keep the extent of the resulting
#'   `SpatRaster`. On `FALSE`, [terra::trim()] is called so the extent may
#'   differ from the extent of the output. See also [drop_na.SpatRaster()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::filter()] method.
#'
#' ## `SpatRaster`
#'
#' Cells that do not meet the conditions on `...` are returned with value
#' `NA`. On a multi-layer `SpatRaster` the `NA` is propagated across all the
#' layers.
#'
#' If `.keep_extent = TRUE` the returned `SpatRaster` has the same CRS, extent,
#' resolution and number of cells as `.data`. If
#' `.keep_extent = FALSE` the outer `NA` cells are trimmed with [terra::trim()],
#' so the extent and number of cells may differ. The output will still have
#' the same CRS and resolution as `.data`.
#'
#' `x` and `y` variables, the longitude and latitude of the `SpatRaster`,
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
  filtered <- dplyr::filter(values, ..., .preserve = .preserve)

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
#' @encoding UTF-8
#' @rdname filter.Spat
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' glimpse(v)
#' v |> filter(cpro < 10)
#'
#' # Same as
#' v |> filter_out(cpro >= 10)
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

#' @export
#' @encoding UTF-8
#' @rdname filter.Spat
#' @importFrom dplyr filter_out
filter_out.SpatVector <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  # Use own method
  tbl <- as_tibble(.data)

  var_index <- make_safe_index("tterra_index", tbl)
  tbl[[var_index]] <- seq_len(nrow(tbl))

  filtered <- dplyr::filter_out(
    tbl,
    ...,
    .by = {{ .by }},
    .preserve = .preserve
  )

  vend <- .data[as.integer(filtered[[var_index]]), ]

  vend <- group_prepare_spat(vend, filtered)

  vend
}

#' @export
dplyr::filter_out
