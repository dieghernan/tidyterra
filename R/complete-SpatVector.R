#' Complete missing combinations in a `SpatVector`
#'
#' @description
#'
#' `complete()` turns implicit missing combinations in a `SpatVector`
#' into explicit rows while preserving geometry and spatial metadata.
#'
#' @rdname complete.SpatVector
#' @name complete.SpatVector
#'
#' @seealso [tidyr::complete()].
#'
#' @family tidyr.missing
#'
#' @importFrom tidyr complete
#'
#' @inheritParams tidyr::complete
#'
#' @param data A `SpatVector`.
#' @returns A `SpatVector` object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::complete()] method for
#' `SpatVector` objects.
#'
#' `complete()` preserves the geometry column while expanding missing
#' combinations. New combinations receive empty geometries.
#'
#' @encoding UTF-8
#' @export
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v <- dplyr::mutate(v, grp = ifelse(iso2 %in% c("ES-AV", "ES-BU"), "a", "b"))
#'
#' complete(v, grp, tidyr::nesting(iso2, name)) |>
#'   glimpse()
#'
complete.SpatVector <- function(data, ..., fill = list(), explicit = TRUE) {
  tbl <- as_tbl_internal(data)

  completed <- tidyr::complete(tbl, ..., fill = fill, explicit = explicit)

  completed <- restore_attr(completed, tbl)

  vend <- as_spat_internal(completed)
  vend <- group_prepare_spat(vend, completed)

  vend
}

#' @export
tidyr::complete
