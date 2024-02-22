#' Coerce a `SpatVector` to a \CRANpkg{sf} object
#'
#' @description
#'
#' [as_sf()] turns a `SpatVector` to \CRANpkg{sf}. This is a wrapper of
#' [sf::st_as_sf()] with the particularity that the groups created with
#' [group_by.SpatVector()] are preserved.
#'
#' @return
#' A \CRANpkg{sf} object.
#'
#' @export
#'
#' @param x A `SpatVector`.
#'
#' @param ... additional arguments passed on to [sf::st_as_sf()].
#'
#' @family coerce
#'
#' @examples
#'
#' library(terra)
#'
#' f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#' v <- terra::vect(f)
#'
#' # This is ungrouped
#' v
#' is_grouped_spatvector(v)
#'
#' # Get an ungrouped data
#' a_sf <- as_sf(v)
#'
#' dplyr::is_grouped_df(a_sf)
#'
#' # Grouped
#'
#' v$gr <- c("C", "A", "A", "B", "A", "B", "B")
#' v$gr2 <- rep(c("F", "G", "F"), 3)
#'
#' gr_v <- group_by(v, gr, gr2)
#'
#' gr_v
#' is_grouped_spatvector(gr_v)
#'
#' group_data(gr_v)
#'
#' # A sf
#'
#' a_gr_sf <- as_sf(gr_v)
#'
#' dplyr::is_grouped_df(a_gr_sf)
#'
#' group_data(a_gr_sf)
#'
as_sf <- function(x, ...) {
  if (!inherits(x, "SpatVector")) {
    cli::cli_abort("{.arg x} is a {.cls {class(x)}} not a {.cls SpatVector}")
  }
  sfobj <- sf::st_as_sf(x, ...)

  if (is_grouped_spatvector(x)) {
    vars <- group_vars(x)
    sfobj <- dplyr::group_by(sfobj, across_all_of(vars))
  }

  if (is_rowwise_spatvector(x)) {
    vars <- group_vars(x)
    sfobj <- dplyr::rowwise(sfobj, dplyr::all_of(vars))
  }

  return(sfobj)
}
