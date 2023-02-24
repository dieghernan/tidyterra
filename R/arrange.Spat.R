#' Order SpatVectors using column values
#'
#' @description
#' `arrange()` orders the geometries of a SpatVector by the values of
#' selected columns.
#'
#' @export
#' @rdname arrange.Spat
#' @name arrange.Spat
#'
#' @seealso [dplyr::arrange()]
#'
#' @family dplyr.methods
#' @family single table verbs
#'
#' @importFrom dplyr arrange
#'
#' @inheritParams distinct.Spat
#' @param ... <[`data-masking`][dplyr::arrange]> Variables, or functions of
#'   variables. Use [dplyr::desc()] to sort a variable in descending order.
#' @param .by_group Ignored by this method
#'
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' `terra_vector[order(terra_vector$variable), ]`
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
  # Use sf method
  sf_obj <- sf::st_as_sf(.data)
  arranged <- dplyr::arrange(sf_obj, ..., .by_group = FALSE)

  return(terra::vect(arranged))
}


#' @export
dplyr::arrange
