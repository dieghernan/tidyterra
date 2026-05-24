#' Row operations for `SpatVector` objects
#'
#' @description
#' Methods for the [dplyr::rows_insert()] family on `SpatVector` objects.
#'
#' @export
#' @encoding UTF-8
#' @rdname rows.SpatVector
#' @name rows.SpatVector
#'
#' @seealso [dplyr::rows_insert()]
#'
#' @family dplyr.rows
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr rows_insert
#'
#' @inheritParams dplyr::rows_insert
#'
#' @param x A `SpatVector`.
#' @param y A data frame, `sf` object or `SpatVector`.
#' @returns A `SpatVector`.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::rows_insert()] family for
#' `SpatVector` objects.
#'
#' ## `SpatVector`
#'
#' Row operations update attributes while preserving the geometry column. When
#' inserting data frame rows without geometry, the output contains empty
#' geometries for the new rows.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' rows_update(
#'   v,
#'   tibble::tibble(cpro = "05", name = "New name"),
#'   by = "cpro"
#' )
#'
#' rows_insert(
#'   v,
#'   tibble::tibble(cpro = "99", name = "New province"),
#'   by = "cpro"
#' )
rows_insert.SpatVector <- function(
  x,
  y,
  by = NULL,
  ...,
  conflict = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
) {
  rows_apply_spat(
    x,
    y,
    dplyr::rows_insert,
    by = by,
    ...,
    conflict = conflict,
    copy = copy,
    in_place = in_place,
    empty_geometry = TRUE
  )
}

#' @export
dplyr::rows_insert

#' @export
#' @encoding UTF-8
#' @rdname rows.SpatVector
#' @importFrom dplyr rows_append
rows_append.SpatVector <- function(x, y, ..., copy = FALSE, in_place = FALSE) {
  rows_apply_spat(
    x,
    y,
    dplyr::rows_append,
    ...,
    copy = copy,
    in_place = in_place,
    empty_geometry = TRUE
  )
}

#' @export
dplyr::rows_append

#' @export
#' @encoding UTF-8
#' @rdname rows.SpatVector
#' @importFrom dplyr rows_update
rows_update.SpatVector <- function(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
) {
  rows_apply_spat(
    x,
    y,
    dplyr::rows_update,
    by = by,
    ...,
    unmatched = unmatched,
    copy = copy,
    in_place = in_place
  )
}

#' @export
dplyr::rows_update

#' @export
#' @encoding UTF-8
#' @rdname rows.SpatVector
#' @importFrom dplyr rows_patch
rows_patch.SpatVector <- function(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
) {
  rows_apply_spat(
    x,
    y,
    dplyr::rows_patch,
    by = by,
    ...,
    unmatched = unmatched,
    copy = copy,
    in_place = in_place
  )
}

#' @export
dplyr::rows_patch

#' @export
#' @encoding UTF-8
#' @rdname rows.SpatVector
#' @importFrom dplyr rows_upsert
rows_upsert.SpatVector <- function(
  x,
  y,
  by = NULL,
  ...,
  copy = FALSE,
  in_place = FALSE
) {
  rows_apply_spat(
    x,
    y,
    dplyr::rows_upsert,
    by = by,
    ...,
    copy = copy,
    in_place = in_place,
    empty_geometry = TRUE
  )
}

#' @export
dplyr::rows_upsert

#' @export
#' @encoding UTF-8
#' @rdname rows.SpatVector
#' @importFrom dplyr rows_delete
rows_delete.SpatVector <- function(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
) {
  rows_apply_spat(
    x,
    y,
    dplyr::rows_delete,
    by = by,
    ...,
    unmatched = unmatched,
    copy = copy,
    in_place = in_place
  )
}

#' @export
dplyr::rows_delete

rows_apply_spat <- function(
  x,
  y,
  fun,
  ...,
  empty_geometry = FALSE
) {
  x_tbl <- as_tbl_internal(x)
  y_tbl <- rows_y_tbl(y, x, empty_geometry = empty_geometry)

  out <- fun(x_tbl, y_tbl, ...)
  out <- restore_attr(out, x_tbl)

  spat <- as_spat_internal(out)
  group_prepare_spat(spat, out)
}

rows_y_tbl <- function(y, template, empty_geometry = FALSE) {
  if (inherits(y, "sf")) {
    y <- as_spatvector(y)
  }

  if (inherits(y, "SpatVector")) {
    y <- crs_compare(y, template, 2)
    return(as_tbl_internal(y))
  }

  y <- as_tibble(y)

  if (empty_geometry && !"geometry" %in% names(y)) {
    y$geometry <- NA_character_
  }

  if ("geometry" %in% names(y)) {
    attr(y, "source") <- "SpatVector"
    attr(y, "crs") <- terra::crs(template)
    attr(y, "geomtype") <- terra::geomtype(template)
  }

  y
}
