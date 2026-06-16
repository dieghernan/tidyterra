#' A grouped `SpatVector`
#'
#' @description
#' The easiest way to create a grouped `SpatVector` is to call the
#' `group_by()` method on a `SpatVector`: this will take care of capturing
#' the unevaluated expressions for you. See [group_by.SpatVector()] for details.
#'
#' This function is the adapted version of [dplyr::is_grouped_df()].
#'
#' See also [group_data.SpatVector()] for the accessory functions that retrieve
#' various metadata from a grouped `SpatVector`.
#'
#' @export
#' @encoding UTF-8
#' @keywords internal
#' @family helpers
#'
#' @inheritParams as_sf
#'
#' @returns `TRUE` if `x` is a grouped `SpatVector`, otherwise `FALSE`.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' is_grouped_spatvector(v)
#'
#' grouped <- group_by(v, iso2)
#'
#' is_grouped_spatvector(grouped)
#'
is_grouped_spatvector <- function(x) {
  att <- attributes(x)

  if (all(att$tblclass == "grouped_df", inherits(att$groups, "tbl_df"))) {
    return(TRUE)
  }

  FALSE
}
