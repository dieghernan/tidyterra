#' Mutating joins for `SpatVector` objects
#'
#' @description
#' Mutating joins add columns from `y` to `x`, matching observations based on
#' the keys. The four mutating joins are: inner join, left join, right join and
#' full join.
#'
#' See [dplyr::inner_join()] for details.
#'
#' @export
#' @encoding UTF-8
#' @rdname mutate-joins.SpatVector
#' @name mutate-joins.SpatVector
#'
#' @seealso
#' [dplyr::inner_join()], [dplyr::left_join()], [dplyr::right_join()],
#' [dplyr::full_join()], [terra::merge()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr inner_join
#'
#' @inheritParams dplyr::full_join
#' @inheritParams as_sf
#'
#' @param y A data frame or other object coercible to a data frame. If a
#'   `SpatVector` or `sf` object is provided, this method returns an error. See
#'   [terra::intersect()] for spatial joins.
#'
#' @returns A `SpatVector` object.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::merge()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::inner_join()] family
#'
#' ## `SpatVector`
#'
#' The geometry column has sticky behavior. This means that the result always
#' has the geometry of `x` for the records that match the join conditions.
#'
#' For [right_join()] and [full_join()], empty geometries may be returned
#' (since `y` is expected to be a data frame with no
#' geometries). Although these join operations are not common in spatial
#' workflows, the function may crash because handling of `EMPTY`
#' geometries differs between \CRANpkg{terra} and \CRANpkg{sf}.
#'
#' @examples
#' library(terra)
#' library(ggplot2)
#' # Vector
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' # A data frame
#' df <- data.frame(
#'   cpro = sprintf("%02d", 1:10),
#'   x = runif(10),
#'   y = runif(10),
#'   letter = rep_len(LETTERS[1:3], length.out = 10)
#' )
#'
#' # Inner join
#' inner <- v |> inner_join(df)
#'
#' nrow(inner)
#' autoplot(inner, aes(fill = letter)) + labs(title = "Inner Join")
#'
#' # Left join
#'
#' left <- v |> left_join(df)
#' nrow(left)
#'
#' autoplot(left, aes(fill = letter)) + labs(title = "Left Join")
#'
#' \donttest{
#' # Right join
#' right <- v |> right_join(df)
#' nrow(right)
#'
#' autoplot(right, aes(fill = letter)) + labs(title = "Right Join")
#'
#' # There are empty geometries, check with data from df
#' ggplot(right, aes(x, y)) +
#'   geom_point(aes(color = letter))
#'
#' # Full join
#' full <- v |> full_join(df)
#' nrow(full)
#'
#' autoplot(full, aes(fill = letter)) + labs(title = "Full Join")
#'
#' # Check with data from df
#' ggplot(full, aes(x, y)) +
#'   geom_point(aes(color = letter))
#' }
inner_join.SpatVector <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::inner_join(
    x_tbl,
    y = y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  joined
}

#' @export
dplyr::inner_join

#' @export
#' @encoding UTF-8
#' @name mutate-joins.SpatVector
#' @importFrom dplyr left_join
left_join.SpatVector <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::left_join(
    x_tbl,
    y = y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  joined
}

#' @export
dplyr::left_join

#' @export
#' @encoding UTF-8
#' @name mutate-joins.SpatVector
#' @importFrom dplyr right_join
right_join.SpatVector <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::right_join(
    x_tbl,
    y = y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  joined
}

#' @export
dplyr::right_join

#' @export
#' @encoding UTF-8
#' @name mutate-joins.SpatVector
#' @importFrom dplyr full_join
full_join.SpatVector <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::full_join(
    x_tbl,
    y = y,
    by = by,
    copy = copy,
    suffix = suffix,
    ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  joined
}

#' @export
dplyr::full_join

#' Filtering joins for `SpatVector` objects
#'
#' @description
#' Filtering joins filter rows from `x` based on the presence or absence of
#' matches in `y`:
#' - [semi_join()] return all rows from `x` with a match in `y`.
#' - [anti_join()] return all rows from `x` without a match in `y`.
#'
#' See [dplyr::semi_join()] for details.
#'
#' @export
#' @encoding UTF-8
#' @rdname filter-joins.SpatVector
#' @name filter-joins.SpatVector
#'
#' @seealso [dplyr::semi_join()], [dplyr::anti_join()], [terra::merge()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr semi_join
#'
#' @inheritParams mutate-joins.SpatVector
#'
#' @returns A `SpatVector` object.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::merge()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::semi_join()] family
#'
#' ## `SpatVector`
#'
#' The geometry column has sticky behavior. This means that the result always
#' has the geometry of `x` for the records that match the join conditions.
#'
#' @examples
#' library(terra)
#' library(ggplot2)
#'
#' # Vector
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' # A data frame
#' df <- data.frame(
#'   cpro = sprintf("%02d", 1:10),
#'   x = runif(10),
#'   y = runif(10),
#'   letter = rep_len(LETTERS[1:3], length.out = 10)
#' )
#'
#' v
#'
#' # Semi join
#' semi <- v |> semi_join(df)
#'
#' semi
#'
#' autoplot(semi, aes(fill = iso2)) + labs(title = "Semi Join")
#'
#' # Anti join
#'
#' anti <- v |> anti_join(df)
#'
#' anti
#'
#' autoplot(anti, aes(fill = iso2)) + labs(title = "Anti Join")
#'
semi_join.SpatVector <- function(x, y, by = NULL, copy = FALSE, ...) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::semi_join(x_tbl, y = y, by = by, copy = copy, ...)

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  joined
}

#' @export
dplyr::semi_join

#' @export
#' @encoding UTF-8
#' @name filter-joins.SpatVector
#' @importFrom dplyr anti_join
anti_join.SpatVector <- function(x, y, by = NULL, copy = FALSE, ...) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::anti_join(x_tbl, y = y, by = by, copy = copy, ...)

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  joined
}

#' @export
dplyr::anti_join

error_spat_join <- function(y) {
  if (inherits(y, c("SpatVector", "sf"))) {
    cli::cli_abort(paste(
      "{.arg y} must not be {.cls {class(y)}}. For spatial joins, use",
      "{.fun terra::intersect}."
    ))
  }
}
