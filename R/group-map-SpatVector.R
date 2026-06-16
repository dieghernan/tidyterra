#' Apply a function to each `SpatVector` group
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' [dplyr::group_map()] and [dplyr::group_modify()] are purrr-style functions
#' that can be used to iterate on grouped `SpatVector` objects.
#'
#' @details
#' Each conceptual group is exposed to `.f` with two pieces of information:
#' `.x`, the subset of rows for the group as a `SpatVector`, and `.y`, a one-row
#' tibble with one column per grouping variable that identifies the group.
#'
#' These methods also work on ungrouped `SpatVector` objects. In that case,
#' `.f` is applied to the entire object and `.y` is a one-row tibble with no
#' columns.
#'
#' @export
#' @encoding UTF-8
#' @rdname group_map.SpatVector
#' @name group_map.SpatVector
#'
#' @keywords internal
#' @seealso
#' [dplyr::group_map()] and [dplyr::group_modify()].
#'
#' @family dplyr.group_functions
#'
#' @importFrom dplyr group_map
#'
#' @inheritParams dplyr::group_map
#'
#' @param .data A grouped or ungrouped `SpatVector`.
#' @param .f A function called with `.x`, a `SpatVector` containing the rows
#'   for one group, and `.y`, a tibble with the group keys.
#' @returns
#' - `group_map()` returns a list of results from calling `.f` on each group.
#' - `group_modify()` returns a `SpatVector`. In that case, `.f` must return
#'   `SpatVector` objects.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_map()] family for
#' `SpatVector` objects.
#'
#' ## `SpatVector`
#'
#' `group_map()` applies `.f` to each group and returns a list.
#' `group_modify()` requires `.f` to return `SpatVector` objects and binds the
#' results.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v$grp <- rep(c("A", "B"), length.out = nrow(v))
#'
#' group_map(group_by(v, grp), ~ nrow(.x))
#'
#' group_modify(group_by(v, grp), ~ mutate(.x, key = .y$grp))
group_map.SpatVector <- function(.data, .f, ..., .keep = FALSE) {
  .f <- rlang::as_function(.f)

  keys <- dplyr::group_keys(.data)
  groups <- group_split(.data, .keep = .keep)

  Map(function(.x, .y) .f(.x, .y, ...), groups, split_keys(keys))
}

#' @export
dplyr::group_map

#' @export
#' @encoding UTF-8
#' @rdname group_map.SpatVector
#' @importFrom dplyr group_modify
group_modify.SpatVector <- function(.data, .f, ..., .keep = FALSE) {
  mapped <- group_map(.data, .f, ..., .keep = .keep)

  if (length(mapped) == 0) {
    return(.data[0, ])
  }

  if (!all(vapply(mapped, inherits, logical(1), "SpatVector"))) {
    cli::cli_abort(paste0(
      "{.fun group_modify.SpatVector} requires {.arg .f} to return ",
      "{.cls SpatVector} objects."
    ))
  }

  bind_spat_rows(mapped)
}

#' @export
dplyr::group_modify

split_keys <- function(keys) {
  if (nrow(keys) == 0) {
    return(list(tibble::tibble()))
  }

  lapply(seq_len(nrow(keys)), function(i) keys[i, , drop = FALSE])
}
