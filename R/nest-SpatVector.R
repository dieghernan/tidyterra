#' Nest `SpatVector` rows
#'
#' @description
#'
#' `nest()` creates list-columns of `SpatVector` objects.
#'
#' @param .data A `SpatVector`.
#' @inheritParams tidyr::nest
#'
#' @return A tibble with one or more list-columns of `SpatVector` objects.
#'
#' @seealso [tidyr::nest()]
#'
#' @family tidyr.nest
#' @family tidyr.methods
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::nest()] method.
#'
#' ## `SpatVector`
#'
#' The geometry column must be nested with the other attributes that form each
#' nested `SpatVector`. These nested list-columns contain `SpatVector` objects
#' and cannot be passed directly to [tidyr::unnest()].
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' nest(v, .by = cpro)
#'
#' v |>
#'   group_by(cpro) |>
#'   nest()
#'
#' @export
#' @encoding UTF-8
#' @rdname nest.SpatVector
#' @importFrom tidyr nest
nest.SpatVector <- function(
  .data,
  ...,
  .by = NULL,
  .key = NULL,
  .names_sep = NULL
) {
  tbl <- as_tbl_internal(.data)

  nested <- tidyr::nest(
    tbl,
    ...,
    .by = {{ .by }},
    .key = .key,
    .names_sep = .names_sep
  )

  list_cols <- names(nested)[vapply(nested, is.list, logical(1))]
  spat_cols <- list_cols[vapply(nested[list_cols], is_nested_spat, logical(1))]

  if (length(spat_cols) == 0) {
    cli::cli_abort(paste(
      "The {.val geometry} column must be nested to create",
      "{.cls SpatVector} list-columns."
    ))
  }

  for (col in spat_cols) {
    nested[[col]] <- lapply(nested[[col]], function(x) {
      as_spat_internal(restore_attr(x, tbl))
    })
  }

  nested
}

#' @export
tidyr::nest

is_nested_spat <- function(x) {
  all(vapply(x, function(y) {
    is.data.frame(y) && "geometry" %in% names(y)
  }, logical(1)))
}
