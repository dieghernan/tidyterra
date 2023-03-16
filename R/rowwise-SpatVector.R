#' Group SpatVector by rows
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `rowwise()` allows you to compute on a SpatVector a row-at-a-time.
#' This is most useful when a vectorised function doesn't exist.
#'
#' Most dplyr verbs implementation in \pkg{tidyterra} preserve row-wise
#' grouping, with the exception of [summarise.SpatVector()]. You can explicitly
#' ungroup with [ungroup.SpatVector()] or [as_tibble()], or convert to a
#' grouped SpatVector with [group_by.SpatVector()].
#'
#' @export
#' @rdname rowwise.SpatVector
#' @name rowwise.SpatVector
#'
#' @seealso [dplyr::rowwise()]
#'
#' @family dplyr.groups
#' @family dplyr.methods
#'
#' @importFrom dplyr rowwise
#' @param data,x A SpatVector object. See **Methods**.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Variables to be
#'   preserved when calling [summarise.SpatVector()]. This is typically a set
#'   of variables whose combination uniquely identify each row. See
#'   [dplyr::rowwise()].
#'
#' @return A SpatVector object with an additional attribute.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::rowwise()] function for
#' SpatVectors.
#'
#' **When mixing** \pkg{terra} **and** \pkg{dplyr} **syntax** on a row-wise
#' SpatVector (i.e, subsetting a SpatVector like `v[1:3,1:2]`) the `groups`
#' attribute can be corrupted. \pkg{tidyterra} would try to re-generate the
#' SpatVector. This would be triggered the next time you use a dplyr verb on
#' your SpatVector.
#'
#' Note also that some operations (as `terra::spatSample()`) would create a new
#' SpatVector. In these cases, the result won't preserve the `groups` attribute.
#' Use [rowwise.SpatVector()] to re-group.
#'
#' @details
#'
#' See **Details** on [dplyr::rowwise()].
#'
#' @examples
#' library(terra)
#' library(dplyr)
#'
#' v <- terra::vect(system.file("shape/nc.shp", package = "sf"))
#'
#' # Select new births
#' nb <- v %>%
#'   select(starts_with("NWBIR")) %>%
#'   glimpse()
#'
#' # Compute the mean of NWBIR on each geometry
#' nb %>%
#'   rowwise() %>%
#'   mutate(nb_mean = mean(c(NWBIR74, NWBIR79)))
#'
#'
#' # use c_across() to more easily select many variables
#' nb %>%
#'   rowwise() %>%
#'   mutate(m = mean(c_across(NWBIR74:NWBIR79)))
#'
#' # Compute the minimum of x and y in each row
#'
#' nb %>%
#'   rowwise() %>%
#'   mutate(min = min(c_across(NWBIR74:NWBIR79)))
#'
#' # Summarising
#' v %>%
#'   rowwise() %>%
#'   summarise(mean_bir = mean(BIR74, BIR79)) %>%
#'   glimpse() %>%
#'   autoplot(aes(fill = mean_bir))
#'
#' # Supply a variable to be kept
#' v %>%
#'   mutate(id2 = as.integer(CNTY_ID / 100)) %>%
#'   rowwise(id2) %>%
#'   summarise(mean_bir = mean(BIR74, BIR79)) %>%
#'   glimpse() %>%
#'   autoplot(aes(fill = as.factor(id2)))
rowwise.SpatVector <- function(data, ...) {
  # Use own method
  x <- data

  data <- as_tibble(data)

  # Add rowwise
  newrow <- dplyr::rowwise(data, ...)

  regen <- cbind(x[, 0], newrow)

  # Add groups/rows metadata
  regen <- group_prepare_spat(regen, newrow)

  regen
}

#' @export
dplyr::rowwise


# Helpers
# Not exported from dplyr, create
is_rowwise_df <- function(x) {
  inherits(x, "rowwise_df")
}


is_rowwise_spatvector <- function(x) {
  att <- attributes(x)

  if (all(att$tblclass == "rowwise_df", inherits(att$groups, "tbl_df"))) {
    return(TRUE)
  }

  return(FALSE)
}
