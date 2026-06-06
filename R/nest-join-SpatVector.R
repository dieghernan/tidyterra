#' Nest join `SpatVector` objects
#'
#' @description
#' `nest_join()` returns a tibble with the attributes and geometry of `x`, plus
#' a list-column containing matching rows from `y`.
#'
#' @export
#' @encoding UTF-8
#' @rdname nest_join.SpatVector
#' @name nest_join.SpatVector
#'
#' @seealso [dplyr::nest_join()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr nest_join
#'
#' @inheritParams dplyr::nest_join
#'
#' @param x A `SpatVector`.
#' @param y A data frame. Spatial `y` inputs are not supported, use spatial
#'   joins from \CRANpkg{terra} for that workflow.
#' @returns A [tibble][tibble::tbl_df].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::nest_join()] method.
#'
#' ## `SpatVector`
#'
#' The output is a tibble with the attributes and WKT geometry of `x`, plus a
#' list-column with matching rows from `y`.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' extra <- tibble::tibble(cpro = c("05", "09"), value = c(1, 2))
#'
#' nest_join(v, extra, by = "cpro")
nest_join.SpatVector <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  keep = NULL,
  name = NULL,
  ...,
  na_matches = c("na", "never")
) {
  error_spat_join(y)

  if (is.null(name)) {
    name <- "data"
  }

  out <- dplyr::nest_join(
    as_tbl_internal(x),
    y = as.data.frame(y),
    by = by,
    copy = copy,
    keep = keep,
    name = name,
    ...,
    na_matches = na_matches
  )

  restore_attr(out, as_tbl_internal(x))
}

#' @export
dplyr::nest_join
