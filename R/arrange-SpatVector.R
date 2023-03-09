#' Order SpatVectors using column values
#'
#' @description
#' `arrange()` orders the geometries of a SpatVector by the values of
#' selected columns.
#'
#' @export
#' @rdname arrange.SpatVector
#' @name arrange.SpatVector
#'
#' @seealso [dplyr::arrange()]
#'
#' @family single table verbs
#' @family dplyr.rows
#' @family dplyr.methods
#'
#' @importFrom dplyr arrange
#'
#' @inheritParams distinct.SpatVector
#' @param ... <[`data-masking`][dplyr::arrange]> Variables, or functions of
#'   variables. Use [dplyr::desc()] to sort a variable in descending order.
#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped SpatVectors only.
#'
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' [terra::sort()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::arrange()] function for
#' SpatVectors.
#'
#'
#' @examples
#'
#' library(terra)
#' library(dplyr)
#'
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#'
#' # Single variable
#'
#' v %>%
#'   arrange(desc(iso2))
#'
#'
#' # Two variables
#' v %>%
#'   mutate(even = as.double(cpro) %% 2 == 0, ) %>%
#'   arrange(desc(even), desc(iso2))
#'
#'
#' # With new variables
#' v %>%
#'   mutate(area_geom = terra::expanse(v)) %>%
#'   arrange(area_geom)
arrange.SpatVector <- function(.data, ..., .by_group = FALSE) {
  # Use own method

  tbl <- as_tbl_internal(.data)
  arranged <- dplyr::arrange(tbl, ..., .by_group = .by_group)

  # Regenerate
  arranged <- restore_attr(arranged, tbl)
  v <- as_spat_internal(arranged)
  v <- group_prepare_spat(v, arranged)

  return(v)
}


#' @export
dplyr::arrange
