#' Mutating joins for SpatVectors
#'
#' @description
#' Mutating joins add columns from `y` to `x`, matching observations based on
#' the keys. There are four mutating joins: the inner join, and the three outer
#' joins.
#'
#' See [dplyr::inner_join()] for details.
#'
#' @export
#' @rdname mutate-joins.SpatVector
#' @name mutate-joins.SpatVector
#'
#' @seealso [dplyr::inner_join()], [dplyr::left_join()], [dplyr::right_join()],
#' [dplyr::full_join()], [terra::merge()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr inner_join
#'
#' @param x A SpatVector created with [terra::vect()].
#' @param y A data frame or other object coercible to a data frame. **If a
#'   SpatVector of sf object** is provided it would return an error (see
#'   [terra::intersect()] for performing spatial joins).
#'
#' @inheritParams dplyr::full_join
#'
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' [terra::merge()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::inner_join()] family
#'
#' ## SpatVector
#'
#' The geometry column has a sticky behavior. This means that the result would
#' have always the geometry of `x` for the records that matches the join
#' conditions.
#'
#' Note that for [right_join()] and [full_join()] it is possible to return
#' empty geometries (since `y` is expected to be a data frame with no
#' geometries). Although this kind of joining operations may not be common on
#' spatial manipulation, it is possible that the function crashes, since
#' handling of `EMPTY` geometries differs on \pkg{terra} and \pkg{sf} (the
#' backend of `*_join.SpatVector()` is the implementation made on \pkg{sf}).
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
#' inner <- v %>% inner_join(df)
#'
#' nrow(inner)
#' autoplot(inner, aes(fill = letter)) + ggtitle("Inner Join")
#'
#'
#' # Left join
#'
#' left <- v %>% left_join(df)
#' nrow(left)
#'
#' autoplot(left, aes(fill = letter)) + ggtitle("Left Join")
#'
#'
#' # Right join
#' right <- v %>% right_join(df)
#' nrow(right)
#'
#' autoplot(right, aes(fill = letter)) + ggtitle("Right Join")
#'
#' # There are empty geometries, check with data from df
#' ggplot(right, aes(x, y)) +
#'   geom_point(aes(color = letter))
#'
#'
#' # Full join
#' full <- v %>% full_join(df)
#' nrow(full)
#'
#' autoplot(full, aes(fill = letter)) + ggtitle("Full Join")
#'
#' # Check with data from df
#' ggplot(full, aes(x, y)) +
#'   geom_point(aes(color = letter))
#'
inner_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ..., keep = NULL) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::inner_join(x_tbl,
    y = y, by = by,
    copy = copy, suffix = suffix, ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  return(joined)
}

#' @export
dplyr::inner_join

#' @importFrom dplyr left_join
#' @export
#' @name mutate-joins.SpatVector
left_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ..., keep = NULL) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::left_join(x_tbl,
    y = y, by = by,
    copy = copy, suffix = suffix, ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  return(joined)
}

#' @export
dplyr::left_join

#' @importFrom dplyr right_join
#' @export
#' @name mutate-joins.SpatVector
right_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ..., keep = NULL) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::right_join(x_tbl,
    y = y, by = by,
    copy = copy, suffix = suffix, ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  return(joined)
}

#' @export
dplyr::right_join


#' @importFrom dplyr full_join
#' @export
#' @name mutate-joins.SpatVector
full_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ..., keep = NULL) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::full_join(x_tbl,
    y = y, by = by,
    copy = copy, suffix = suffix, ...,
    keep = keep
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  return(joined)
}

#' @export
dplyr::full_join


#' Filtering joins for SpatVectors
#'
#' @description
#' Filtering joins filter rows from `x` based on the presence or absence of
#' matches in `y`:
#'  - [semi_join()] return all rows from `x` with a match in `y`.
#'  - [anti_join()] return all rows from `x` without a match in `y`.
#'
#' See [dplyr::semi_join()] for details.
#'
#' @export
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
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' [terra::merge()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::semi_join()] family
#'
#' ## SpatVector
#'
#' The geometry column has a sticky behavior. This means that the result would
#' have always the geometry of `x` for the records that matches the join
#' conditions.
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
#' semi <- v %>% semi_join(df)
#'
#' semi
#'
#' autoplot(semi, aes(fill = iso2)) + ggtitle("Semi Join")
#'
#'
#' # Anti join
#'
#' anti <- v %>% anti_join(df)
#'
#' anti
#'
#' autoplot(anti, aes(fill = iso2)) + ggtitle("Anti Join")
#'
semi_join.SpatVector <- function(x, y, by = NULL, copy = FALSE, ...) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::semi_join(x_tbl,
    y = y, by = by,
    copy = copy, ...
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  return(joined)
}

#' @export
dplyr::semi_join

#' @importFrom dplyr anti_join
#' @export
#' @name filter-joins.SpatVector
anti_join.SpatVector <- function(x, y, by = NULL, copy = FALSE, ...) {
  error_spat_join(y)
  # Use own method
  x_tbl <- as_tbl_internal(x)
  y <- as.data.frame(y)

  joined_tbl <- dplyr::anti_join(x_tbl,
    y = y, by = by,
    copy = copy, ...
  )

  joined_tbl <- restore_attr(joined_tbl, x_tbl)
  joined <- as_spat_internal(joined_tbl)
  joined <- group_prepare_spat(joined, joined_tbl)

  return(joined)
}

#' @export
dplyr::anti_join


error_spat_join <- function(y) {
  if (inherits(y, c("SpatVector", "sf"))) {
    cli::cli_abort(paste(
      "{.arg y} should not be {.cls {class(y)}}. For spatial joins use",
      "{.fun terra::intersect}"
    ))
  }
}
