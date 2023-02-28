#' A grouped SpatVector.
#'
#' @description
#' The easiest way to create a grouped SpatVector is to call the
#' `group_by()` method on a Spatvector: this will take care of capturing
#' the unevaluated expressions for you. See [group_by.SpatVector()] for details.
#'
#' This function is the adapted version of [dplyr::is_grouped_df()].
#'
#' See also [group_data.SpatVector()] for the accessor functions that retrieve
#' various metadata from a grouped SpatVector.
#'
#' @keywords internal
#' @param x a SpatVector.
#'
#'
#' @family helpers
#'
#' @export
is_grouped_spatvector <- function(x) {
  # Check grouping vars
  has_signal <- any(names(x) == "dplyr.group_vars")

  return(has_signal)
}
