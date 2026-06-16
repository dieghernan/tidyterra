#' Trim grouping structure
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This method drops unused levels of all factors that are used as grouping
#' variables and recalculates the grouping structure.
#'
#' `group_trim()` is particularly useful after a [filter()] that is intended
#' to select a subset of groups.
#'
#' @details
#' See **Details** on [dplyr::group_trim()].
#'
#' @export
#' @encoding UTF-8
#' @rdname group_trim.SpatVector
#' @name group_trim.SpatVector
#'
#' @keywords internal
#' @seealso [dplyr::group_trim()].
#'
#' @family dplyr.group_functions
#'
#' @importFrom dplyr group_trim
#'
#' @param .tbl A `SpatVector` object. See **Methods**.
#' @param .drop See [group_by.SpatVector()].
#'
#' @returns A `SpatVector` object with updated grouping metadata.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_trim()] for `SpatVector`
#' objects.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' v$group <- rep(c("A", "B", "C"), 3)
#'
#' v |>
#'   group_by(group) |>
#'   filter(group == "B", .preserve = TRUE) |>
#'   group_trim()
#'
group_trim.SpatVector <- function(.tbl, .drop = group_by_drop_default(.tbl)) {
  trimmed <- dplyr::group_trim(tbl_for_groups(.tbl), .drop = .drop)
  group_prepare_spat(.tbl, trimmed)
}

#' @export
dplyr::group_trim
