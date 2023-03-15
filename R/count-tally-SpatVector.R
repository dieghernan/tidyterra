#' Count the observations in each SpatVector group
#'
#' @description
#' `count()` lets you quickly count the unique values of one or more variables:
#' `df %>% count(a, b)` is roughly equivalent to
#' `df %>% group_by(a, b) %>% summarise(n = n())`.
#' `count()` is paired with `tally()`, a lower-level helper that is equivalent
#' to `df %>% summarise(n = n())`.
#'
#'
#' @export
#' @rdname count.SpatVector
#' @name count.SpatVector
#'
#' @seealso [dplyr::count()], [dplyr::tally()]
#'
#' @family dplyr.groups
#' @family dplyr.methods
#'
#' @importFrom dplyr count
#'
#' @param x A SpatVector.
#' @param wt Not implemented on this method
#' @inheritParams dplyr::count
#' @inheritParams summarise.SpatVector
#'
#' @return A SpatVector object with an additional attribute.
#'
#' @section terra equivalent:
#'
#' [terra::aggregate()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::count()] family functions for
#' SpatVectors.
#'
#' [tally()] will always return a disaggregated geometry while [count()] can
#' handle this. See also [summarise.SpatVector()].
#'
#' @examples
#' \donttest{
#'
#' library(terra)
#' f <- system.file("ex/lux.shp", package = "terra")
#' p <- vect(f)
#'
#'
#' p %>% count(NAME_1, sort = TRUE)
#'
#' p %>% count(NAME_1, sort = TRUE)
#'
#' p %>% count(pop = ifelse(POP < 20000, "A", "B"))
#'
#' # tally() is a lower-level function that assumes you've done the grouping
#' p %>% tally()
#'
#' p %>%
#'   group_by(NAME_1) %>%
#'   tally()
#'
#' # Dissolve geometries by default
#'
#' library(ggplot2)
#' p %>%
#'   count(NAME_1) %>%
#'   ggplot() +
#'   geom_spatvector(aes(fill = n))
#'
#' # Opt out
#' p %>%
#'   count(NAME_1, .dissolve = FALSE, sort = TRUE) %>%
#'   ggplot() +
#'   geom_spatvector(aes(fill = n))
#' }
count.SpatVector <- function(x, ..., wt = NULL, sort = FALSE, name = NULL,
                             .drop = group_by_drop_default(x),
                             .dissolve = TRUE) {
  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE, .drop = .drop)
  } else {
    out <- x
  }
  # Get group template from dplyr method
  template <- dplyr::count(as_tibble(x), ...,
    sort = sort,
    name = name, .drop = .drop
  )

  vend <- tally(out, sort = sort, name = name)


  # Dissolve if requested
  if (.dissolve) {
    keepdf <- as_tibble(vend)
    var_index <- make_safe_index("tterra_index", keepdf)
    df <- data.frame(n = seq_len(nrow(keepdf)))
    names(df) <- var_index
    vend <- cbind(vend[, 0], df)
    vend <- terra::aggregate(vend, by = var_index, dissolve = TRUE)
    vend <- cbind(vend[, 0], keepdf)
  }

  # Ensure groups
  vend <- group_prepare_spat(vend, template)


  vend
}

#' @export
dplyr::count

#' @importFrom dplyr tally
#' @export
#' @name count.SpatVector
tally.SpatVector <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  tbl <- as_tibble(x)
  spatv <- x

  # Use tbl unsorted
  tallyed <- dplyr::tally(tbl, sort = FALSE, name = name)

  if (is_grouped_spatvector(spatv)) {
    var_index <- make_safe_index("tterra_index", spatv)
    df <- data.frame(n = group_indices(spatv))
    names(df) <- var_index
    spatv <- cbind(spatv, df)
    newgeom <- terra::aggregate(spatv, by = var_index, dissolve = FALSE)
  } else {
    newgeom <- terra::aggregate(spatv, dissolve = FALSE)
  }

  vend <- cbind(newgeom[, 0], tallyed)

  # Ensure groups
  vend <- group_prepare_spat(vend, tallyed)

  if (sort) {
    # Arrange
    order_v <- rev(names(vend))[1]
    sort_order <- as_tibble(vend)[[order_v]]
    vend <- vend[order(sort_order, decreasing = TRUE), ]
  }

  vend
}



#' @export
dplyr::tally
