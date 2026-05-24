#' Expand `SpatVector` attribute combinations
#'
#' @description
#' `expand()` returns a tibble with all combinations of selected attributes.
#' It does not return a `SpatVector` because newly created combinations do not
#' have a well-defined geometry. Use [complete.SpatVector()] when empty
#' geometries should be added explicitly.
#'
#' @export
#' @encoding UTF-8
#' @rdname expand.SpatVector
#' @name expand.SpatVector
#'
#' @seealso [tidyr::expand()], [complete.SpatVector()]
#'
#' @family tidyr.missing
#' @family tidyr.methods
#'
#' @importFrom tidyr expand
#'
#' @inheritParams tidyr::expand
#'
#' @param data A `SpatVector`.
#' @returns A [tibble][tibble::tbl_df].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::expand()] method.
#'
#' ## `SpatVector`
#'
#' The output is a tibble with attribute combinations. Geometry is not
#' preserved because new combinations do not have a well-defined geometry.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v$grp <- rep(c("A", "B"), length.out = nrow(v))
#'
#' expand(v, grp, cpro)
expand.SpatVector <- function(data, ..., .name_repair = "check_unique") {
  tbl <- as_tibble(data)
  tidyr::expand(tbl, ..., .name_repair = .name_repair)
}

#' @export
tidyr::expand
