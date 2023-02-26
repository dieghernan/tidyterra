#' TODO
#'
#' @description
#'
#' TODO
#'
#' @export
#' @rdname group-by.SpatVector
#' @name group-by.SpatVector
#'
#' @seealso [dplyr::group_by()], [dplyr::ungroup()]
#'
#' @family dplyr.methods
#' @family single table verbs
#'
#' @importFrom dplyr group_by
#'
group_by.SpatVector <- function(.data, ..., .add = FALSE,
                                .drop = group_by_drop_default(.data)) {
  # Use own method
  .data <- as_tibble(.data, geom = "WKT")

  # Add groups
  newgroups <- dplyr::group_by(.data, ..., .add = .add, .drop = .drop)

  # Add groups metadata
  newgroups$dplyr.group_indices <- dplyr::group_indices(newgroups)
  newgroups$dplyr.group_vars <- NA
  newgroups$dplyr.group_vars[[1]] <- dplyr::group_vars(newgroups)

# TODO: Need to create a function for this on the new version

  grouped <- terra::vect(newgroups, geom = "geometry", crs = attr(newgroups,
                                                                  "crs"))

  grouped
}

#' @export
dplyr::group_by
