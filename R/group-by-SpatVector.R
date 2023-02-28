#' Group a SpatVector by one or more variables
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Most data operations are done on groups defined by variables. [group_by()]
#' takes an existing SpatVector and creates a new variable indicating the
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
#' @family dplyr.methods
#' @family single table verbs
#' @family grouping
#'
#' @importFrom dplyr group_by
#'
#' @param .data,x A SpatVector object. See **Methods**.
#' @inheritParams dplyr::group_by
#'
#' @return A SpatVector object with an additional column `dplyr.group_vars`.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_by()] family functions for
#' SpatVectors.
#'
#' Since the `SpatVector` class is a **S4 class**, there is not a
#' straightforward way to add an additional class (as `grouped_df`) to the
#' object. Instead, the implementation on \pkg{tidyterra} consists on simply
#' create a new column `dplyr.group_vars` where the value for the first row
#' is a representation of the variables declared in `...` and the value for
#' the rest of rows is `NA`.
#'
#' This column is used internally for deriving the requested groups on the
#' subsequent operations. Hence, `grouped_df` class (default behaviour in
#' \pkg{dplyr}) is substituted on this implementation by `x$dplyr.group_vars`.
#'
#' Note that removing `x$dplyr.group_vars` would cause \pkg{tidyterra} to not
#' recognize the SpatVector as grouped. This is better achieved by using the
#' [ungroup()] function.
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
  .data <- as_tibble(.data, geom = "WKT")

  # Add groups
  newgroups <- dplyr::group_by(.data, ..., .add = .add, .drop = .drop)

  # Add groups metadata
  newgroups$dplyr.group_vars <- NA
  newgroups$dplyr.group_vars[[1]] <- paste0(dplyr::group_vars(newgroups),
    collapse = ","
  )

  # TODO: Need to create a function for this on the new version

  grouped <- terra::vect(newgroups, geom = "geometry", crs = attr(
    newgroups,
    "crs"
  ))

  grouped
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
    x <- x[, !grepl("dplyr.group", names(x))]
    return(x)
  }

  # Regenerate grouping and use default method
  old_groups <- group_vars(x)
  tbl <- as_tibble(x)
  g_tbl <- dplyr::group_by(tbl, dplyr::across(dplyr::all_of(old_groups)))

  # Ungroup default method
  newgroups <- dplyr::ungroup(g_tbl, ...)


  # Add groups metadata
  x$dplyr.group_vars <- NA
  x$dplyr.group_vars[[1]] <- paste0(dplyr::group_vars(newgroups),
    collapse = ","
  )


  return(x)
}

#' @export
dplyr::ungroup
