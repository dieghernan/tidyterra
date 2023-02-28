#' Group a SpatVector by one or more variables
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Most data operations are done on groups defined by variables. [group_by()]
#' adds a new attribute to an existing SpatVector indicating the
#' corresponding groups. See **Methods**.
#'
#'
#'
#' @export
#' @rdname group-by.SpatVector
#' @name group-by.SpatVector
#'
#' @seealso [dplyr::group_by()], [dplyr::ungroup()]
#'
#' @family dplyr.groups
#' @family dplyr.methods
#'
#' @importFrom dplyr group_by
#'
#' @param .data,x A SpatVector object. See **Methods**.
#' @inheritParams dplyr::group_by
#'
#' @return A SpatVector object with an additional attribute.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_by()] family functions for
#' SpatVectors.
#'
#'
#' @details
#'
#' See **Details** on [dplyr::group_by()].
#' @examples
#'
#' # TODO
group_by.SpatVector <- function(.data, ..., .add = FALSE,
                                .drop = dplyr::group_by_drop_default(.data)) {
  # Use own method
  x <- .data

  .data <- as_tibble(.data)

  # Add groups
  newgroups <- dplyr::group_by(.data, ..., .add = .add, .drop = .drop)

  regen <- cbind(v[, 0], newgroups)

  # Add groups metadata
  attr(regen, "group_vars") <- dplyr::group_vars(newgroups)

  regen
}

#' @export
dplyr::group_by

#' @importFrom dplyr ungroup
#' @export
#' @name group-by.SpatVector
ungroup.SpatVector <- function(x, ...) {
  # Use own method
  if (!is_grouped_spatvector(x)) {
    return(x)
  }

  # If empty dots undo all groups
  if (rlang::dots_n(...) == 0L) {
    attr(x, "group_vars") <- NULL
    return(x)
  }

  # Regenerate grouping and use default method
  old_groups <- group_vars(x)
  tbl <- as_tibble(x)
  g_tbl <- dplyr::group_by(tbl, dplyr::across(dplyr::all_of(old_groups)))

  # Ungroup default method
  newgroups <- dplyr::ungroup(g_tbl, ...)


  # Add groups metadata
  attr(x, "group_vars") <- dplyr::group_vars(newgroups)

  return(x)
}

#' @export
dplyr::ungroup
