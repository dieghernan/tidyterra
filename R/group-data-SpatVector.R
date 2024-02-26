#' Grouping metadata for `SpatVector` objects
#'
#' @description
#' This collection of functions accesses data about grouped `SpatVector` objects
#' in various ways:
#'
#' * [group_data()] returns a tibble that defines the grouping structure.
#'   The columns give the values of the grouping variables. The last column,
#'   always called `.rows`, is a list of integer vectors that gives the
#'   location of the rows in each group.
#'
#' * [group_keys()] returns a tibble describing the groups.
#'
#' * [group_rows()] returns a list of integer vectors giving the rows that
#'   each group contains.
#'
#' * [group_indices()] returns an integer vector the same length as `.data`
#'   that gives the group that each row belongs to.
#'
#' * [group_vars()] gives names of grouping variables as character vector.
#'
#' * [groups()] gives the names of the grouping variables as a list of symbols.
#'
#' * [group_size()] gives the size of each group.
#'
#' * [n_groups()] gives the total number of groups.
#'
#' See [dplyr::group_data()].
#'
#' @param .data,.tbl,x A `SpatVector`.
#' @inheritParams dplyr::group_data
#'
#' @keywords internal
#'
#' @return
#'
#' See the description of the function. The results are usually tibbles,
#' lists or vectors. These functions does not return `SpatVector` objects.
#'
#' @rdname group_data.SpatVector
#' @name group_data.SpatVector
#'
#' @export
#' @importFrom dplyr group_data
#'
#' @examples
#' library(terra)
#'
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v$gr_1 <- rep_len(c("A", "A", "B"), length.out = nrow(v))
#' v$gr_2 <- rep_len(c("C", "D"), length.out = nrow(v))
#'
#' # Ungrouped
#'
#' n_groups(v)
#'
#' group_vars(v)
#'
#' group_keys(v)
#'
#' group_size(v)
#'
#' groups(v)
#'
#' group_rows(v)
#'
#' group_data(v)
#'
#' group_indices(v)
#'
#' # Grouped by one var
#' gv <- group_by(v, gr_1)
#'
#' n_groups(gv)
#'
#' group_vars(gv)
#'
#' group_keys(gv)
#'
#' group_size(gv)
#'
#' groups(gv)
#'
#' group_rows(gv)
#'
#' group_data(gv)
#'
#' group_indices(gv)
#'
#' # Grouped by several vars
#'
#' gv2 <- group_by(v, gr_1, gr_2)
#'
#' n_groups(gv2)
#'
#' group_vars(gv2)
#'
#' group_keys(gv2)
#'
#' group_size(gv2)
#'
#' groups(gv2)
#'
#' group_rows(gv2)
#'
#' group_data(gv2)
#'
#' group_indices(gv2)
group_data.SpatVector <- function(.data) {
  # Dispatch to dplyr
  dplyr::group_data(tbl_for_groups(.data))
}
#' @export
dplyr::group_data

#' @export
#' @rdname group_data.SpatVector
#' @importFrom dplyr group_keys
group_keys.SpatVector <- function(.tbl, ...) {
  # Dispatch to dplyr
  dplyr::group_keys(tbl_for_groups(.tbl, ...))
}

#' @export
dplyr::group_keys


#' @importFrom dplyr group_rows
#' @export
dplyr::group_rows

#' @export
#' @rdname group_data.SpatVector
#' @importFrom dplyr group_indices
group_indices.SpatVector <- function(.data, ...) {
  # Dispatch to dplyr
  dplyr::group_indices(tbl_for_groups(.data), ...)
}

#' @export
dplyr::group_indices


#' @export
#' @rdname group_data.SpatVector
#' @importFrom dplyr group_vars
group_vars.SpatVector <- function(x) {
  # Dispatch to dplyr
  dplyr::group_vars(tbl_for_groups(x))
}

#' @export
dplyr::group_vars

#' @export
#' @rdname group_data.SpatVector
#' @importFrom dplyr groups
groups.SpatVector <- function(x) {
  # Dispatch to dplyr
  dplyr::groups(tbl_for_groups(x))
}

#' @export
dplyr::groups

#' @export
#' @rdname group_data.SpatVector
#' @importFrom dplyr group_size
group_size.SpatVector <- function(x) {
  # Dispatch to dplyr
  dplyr::group_size(tbl_for_groups(x))
}

#' @export
dplyr::group_size

#' @export
dplyr::groups

#' @export
#' @rdname group_data.SpatVector
#' @importFrom dplyr n_groups
n_groups.SpatVector <- function(x) {
  # Dispatch to dplyr
  dplyr::n_groups(tbl_for_groups(x))
}

#' @export
dplyr::n_groups


# Helper
tbl_for_groups <- function(x) {
  df <- tibble::as_tibble(terra::as.data.frame(x))

  # Grouped
  if (is_grouped_spatvector(x)) {
    # Add class
    class(df) <- c("grouped_df", class(df))
    attr(df, "groups") <- attr(x, "groups")
  }

  # Grouped
  if (is_rowwise_spatvector(x)) {
    # Add class
    class(df) <- c("rowwise_df", class(df))
    attr(df, "groups") <- attr(x, "groups")
  }

  return(df)
}
