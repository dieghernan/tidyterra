#' Coerce a `SpatVector` to a [`sf`][sf::st_sf] object
#'
#' @description
#'
#' [as_sf()] coerces a `SpatVector` into an [`sf`][sf::st_sf] object. It wraps
#' [sf::st_as_sf()] and preserves groups created with
#' [group_by.SpatVector()].
#'
#' @export
#' @encoding UTF-8
#'
#' @family coerce
#'
#' @param x A `SpatVector` created with [terra::vect()].
#'
#' @param ... Additional arguments passed on to [sf::st_as_sf()].
#'
#' @returns
#' A [`sf`][sf::st_sf] object with an additional `tbl_df` class for
#' pretty printing.
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
    cli::cli_abort(
      "{.arg x} must be a {.cls SpatVector}, not {.cls {class(x)}}."
    )
  }
  sfobj <- sf::st_as_sf(x, ...)

  # Make an sf/tibble object.
  # https://github.com/r-spatial/sf/issues/951
  # This boosts performance
  template <- sf::st_as_sf(tibble::tibble(x = 1, y = 1), coords = c("x", "y"))
  class(sfobj) <- class(template)

  if (is_grouped_spatvector(x)) {
    vars <- group_vars(x)
    sfobj <- dplyr::group_by(sfobj, across_all_of(vars))
  }

  if (is_rowwise_spatvector(x)) {
    vars <- group_vars(x)
    sfobj <- dplyr::rowwise(sfobj, dplyr::all_of(vars))
  }

  sfobj
}
