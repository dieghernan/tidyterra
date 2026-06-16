#' Split `SpatVector` by groups
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' [group_split()] works like [base::split()] but:
#'
#' - It uses the grouping structure from [group_by.SpatVector()] and therefore
#'   is subject to the data mask.
#' - It does not name the elements of the list based on the grouping as this
#'   only works well for a single character grouping variable. Instead,
#'   use [group_keys.SpatVector()] to access a data frame that defines the
#'   groups.
#'
#' See [dplyr::group_split()] for more information.
#'
#' @details
#' See **Details** on [dplyr::group_split()].
#'
#' @export
#' @encoding UTF-8
#' @rdname group_split.SpatVector
#'
#' @name group_split.SpatVector
#' @keywords internal
#' @seealso [dplyr::group_split()], [terra::svc()]
#'
#' @family dplyr.group_functions
#'
#' @importFrom dplyr group_split
#' @inheritParams dplyr::group_split
#'
#' @param .tbl A `SpatVector` object. See **Methods**.
#' @param ... If `.tbl` is an ungrouped `SpatVector`, a grouping specification,
#'   forwarded to [group_by.SpatVector()].
#'
#' @returns A list of `SpatVector` objects. Each `SpatVector` contains the rows
#'   of `.tbl` for the associated group and all columns. When `.keep = TRUE`,
#'   the output includes the grouping variables.
#'
#' @section Lifecycle:
#' `group_split()` is not stable because you can achieve very similar results by
#' manipulating the nested column returned from
#' [`nest(.by =)`][nest.SpatVector()]. That also retains the group keys all
#' within a single data structure. `group_split()` may be deprecated in the
#' future.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::svc()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_split()] for `SpatVector`
#' objects.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' v$group <- rep(c("A", "B", "C"), 3)
#'
#' v |>
#'   group_by(group) |>
#'   group_split()
#'
#' # Coerce the result to a SpatVectorCollection.
#' v |>
#'   group_by(group) |>
#'   group_split() |>
#'   terra::svc()
group_split.SpatVector <- function(.tbl, ..., .keep = TRUE) {
  tbl <- as_tibble(.tbl)
  ind <- make_safe_index("tterra_index", tbl)
  tbl[[ind]] <- seq_len(nrow(tbl))

  split_tbls <- dplyr::group_split(tbl, ..., .keep = .keep)

  lapply(split_tbls, function(x) {
    split_index <- as.integer(x[[ind]])
    split_attrs <- x[setdiff(names(x), ind)]

    vend <- cbind(.tbl[split_index, 0], split_attrs)
    group_prepare_spat(vend, split_attrs)
  })
}

#' @export
dplyr::group_split
