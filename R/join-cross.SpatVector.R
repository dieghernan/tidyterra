#' Cross joins for `SpatVector` objects
#'
#' @description
#' Cross joins match each row in `x` to every row in `y`.
#'
#' See [dplyr::cross_join()] for details.
#'
#' @export
#' @encoding UTF-8
#' @rdname cross_join.SpatVector
#' @name cross_join.SpatVector
#'
#' @seealso [dplyr::cross_join()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr cross_join
#'
#' @inheritParams as_sf
#' @inheritParams dplyr::cross_join
#'
#' @param y A data frame or other object coercible to a data frame. If a
#'   `SpatVector` or `sf` object is provided, this method returns an error.
#' @returns A `SpatVector` object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::cross_join()] method.
#'
#' ## `SpatVector`
#'
#' The geometry column has sticky behavior. The result repeats each geometry in
#' `x` once for every row in `y`.
#'
#' If `y` has a column named `geometry`, it is treated as a regular attribute
#' and receives the suffix from `suffix`.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' labels <- data.frame(period = c("past", "present"))
#'
#' cross_join(v, labels)
cross_join.SpatVector <- function(
  x,
  y,
  ...,
  copy = FALSE,
  suffix = c(".x", ".y")
) {
  error_spat_join(y)

  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::cross_join(
    x_tbl,
    y = y,
    ...,
    copy = copy,
    suffix = suffix
  )

  geom_col <- paste0("geometry", suffix[[1]])
  if ("geometry" %in% names(y) && geom_col %in% names(joined_tbl)) {
    names(joined_tbl)[names(joined_tbl) == geom_col] <- "geometry"
  }

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  group_prepare_spat(joined, joined_tbl)
}

#' @export
dplyr::cross_join
