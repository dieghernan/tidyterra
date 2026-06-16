#' Duplicate `SpatVector` rows
#'
#' @description
#'
#' `uncount()` duplicates rows according to a weighting variable.
#'
#' @export
#' @encoding UTF-8
#' @rdname uncount.SpatVector
#'
#' @seealso [tidyr::uncount()]
#'
#' @family tidyr.rows
#' @family tidyr.methods
#'
#' @importFrom tidyr uncount
#'
#' @inheritParams tidyr::uncount
#'
#' @param data A `SpatVector`.
#' @returns A `SpatVector` object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::uncount()] method.
#'
#' ## `SpatVector`
#'
#' Each duplicated row keeps the input geometry.
#'
#' @examples
#' library(tidyr)
#'
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v$copies <- rep_len(1:2, nrow(v))
#'
#' uncount(v, copies)
#'
uncount.SpatVector <- function(data, weights, ..., .remove = TRUE, .id = NULL) {
  tbl <- as_tbl_internal(data)

  uncounted <- tidyr::uncount(
    tbl,
    {{ weights }},
    ...,
    .remove = .remove,
    .id = .id
  )

  if (nrow(uncounted) == 0) {
    attrs <- uncounted[setdiff(names(uncounted), "geometry")]
    vend <- cbind(data[0, 0], as.data.frame(attrs))
    return(group_prepare_spat(vend, uncounted))
  }

  uncounted <- restore_attr(uncounted, tbl)
  vend <- as_spat_internal(uncounted)
  group_prepare_spat(vend, uncounted)
}

#' @export
tidyr::uncount
