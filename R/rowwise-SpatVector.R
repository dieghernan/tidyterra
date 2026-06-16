#' Group `SpatVector` objects by rows
#'
#' @description
#'
#' `rowwise()` lets you compute on a `SpatVector` one row at a time.
#' This is most useful when a vectorised function does not exist.
#'
#' Most \CRANpkg{dplyr} verb implementations in \CRANpkg{tidyterra} preserve
#' row-wise grouping. The exception is [summarise.SpatVector()], which returns
#' a [grouped SpatVector][group_by.SpatVector]. You can explicitly ungroup with
#' [ungroup.SpatVector()] or [as_tibble()] or convert to a grouped `SpatVector`
#' with [group_by.SpatVector()].
#'
#' @details
#'
#' See **Details** on [dplyr::rowwise()].
#'
#' @export
#' @encoding UTF-8
#' @rdname rowwise.SpatVector
#' @name rowwise.SpatVector
#'
#' @seealso [dplyr::rowwise()]
#'
#' @family dplyr.groups
#' @family dplyr.methods
#'
#' @importFrom dplyr rowwise
#' @param data A `SpatVector` object. See **Methods**.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Variables to be
#'   preserved when calling [summarise.SpatVector()]. This is typically a set of
#'   variables whose combination uniquely identifies each row. See
#'   [dplyr::rowwise()].
#'
#'   Unlike [group_by.SpatVector()], you cannot create new variables here.
#'   Instead, you can select multiple variables, for example with
#'   [everything()].
#'
#' @returns The same `SpatVector` object with updated grouping metadata.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::rowwise()] function for
#' `SpatVector` objects.
#'
#' **When mixing** \CRANpkg{terra} **and** \CRANpkg{dplyr} **syntax** on a
#' row-wise `SpatVector`, for example subsetting a `SpatVector` like
#' `v[1:3,1:2]`, the `groups` attribute can be corrupted.
#' \CRANpkg{tidyterra} tries to
#' regenerate the `SpatVector`. This is triggered the next time you use a
#' \CRANpkg{dplyr} verb on your `SpatVector`.
#'
#' Some operations, such as `terra::spatSample()`, create a new `SpatVector`.
#' In these cases, the result does not preserve the `groups` attribute. Use
#' [rowwise.SpatVector()] to re-group.
#'
#' @examples
#' library(terra)
#' library(dplyr)
#'
#' v <- terra::vect(system.file("shape/nc.shp", package = "sf"))
#'
#' # Select new births
#' nb <- v |>
#'   select(starts_with("NWBIR")) |>
#'   glimpse()
#'
#' # Compute the mean of NWBIR on each geometry
#' nb |>
#'   rowwise() |>
#'   mutate(nb_mean = mean(c(NWBIR74, NWBIR79)))
#'
#' # Additional examples
#' \donttest{
#' # Use c_across() to select many variables more easily.
#' nb |>
#'   rowwise() |>
#'   mutate(m = mean(c_across(NWBIR74:NWBIR79)))
#'
#' # Compute the minimum of x and y in each row
#'
#' nb |>
#'   rowwise() |>
#'   mutate(min = min(c_across(NWBIR74:NWBIR79)))
#'
#' # Summarize.
#' v |>
#'   rowwise() |>
#'   summarise(mean_bir = mean(BIR74, BIR79)) |>
#'   glimpse() |>
#'   autoplot(aes(fill = mean_bir))
#'
#' # Supply a variable to be kept
#' v |>
#'   mutate(id2 = as.integer(CNTY_ID / 100)) |>
#'   rowwise(id2) |>
#'   summarise(mean_bir = mean(BIR74, BIR79)) |>
#'   glimpse() |>
#'   autoplot(aes(fill = as.factor(id2)))
#' }
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

  FALSE
}
