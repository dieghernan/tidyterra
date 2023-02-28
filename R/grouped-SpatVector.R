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

  if (isFALSE(has_signal)) {
    return(FALSE)
  }

  # Check that the var field is alright
  gvars <- as.vector(x$dplyr.group_vars)

  gvars <- as.vector(x$dplyr.group_vars)

  gvars_nona <- gvars[!is.na(gvars)]

  ok_length <- length(gvars_nona) == 1
  ok_type <- is.character(gvars)

  # If vars are corrupted return a warning
  if (!all(ok_length, ok_type)) {
    cli::cli_alert_warning(paste0(
      "dplyr.group_vars column corrupted. Can't ",
      "create groups (Maybe use ",
      cli::col_blue("ungroup()"), " method)"
    ))
  }

  return(all(ok_length, ok_type))
}
