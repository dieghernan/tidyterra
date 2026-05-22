#' Trim grouping structure
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Drop unused levels of all factors that are used as grouping variables,
#' then recalculates the grouping structure.
#'
#' `group_trim()` is particularly useful after a [filter()] that is intended
#' to select a subset of groups.
#'
#' @export
#' @encoding UTF-8
#' @rdname group-trim.SpatVector
#' @name group-trim.SpatVector
#'
#' @seealso [dplyr::group_trim()].
#'
#' @family dplyr.groups
#' @family dplyr.methods
#'
#' @importFrom dplyr group_trim
#'
#' @param .tbl A `SpatVector` object. See **Methods**.
#' @param .drop See [group_by.SpatVector()].
#'
#' @return A `SpatVector` object with an additional attribute.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_trim()] for `SpatVector`
#' objects.
#'
#' @details
#' See **Details** on [dplyr::group_by()].
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
#' @importFrom dplyr group_trim
#' @export
#' @encoding UTF-8
group_trim.SpatVector <- function(
  .tbl,
  .drop = group_by_drop_default(.tbl)
) {
  trimmed <- dplyr::group_trim(tbl_for_groups(.tbl), .drop = .drop)
  group_prepare_spat(.tbl, trimmed)
}

#' @export
dplyr::group_trim
