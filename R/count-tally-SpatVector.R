#' Count the observations in each `SpatVector` group
#'
#' @description
#' `count()` lets you quickly count the unique values of one or more variables:
#' - `df %>% count(a, b)` is roughly equivalent to
#'   `df %>% group_by(a, b) %>% summarise(n = n())`.
#' - `count()` is paired with `tally()`, a lower-level helper that is equivalent
#'    to `df %>% summarise(n = n())`.
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
#' @param x A `SpatVector`.
#' @param wt Not implemented on this method
#' @inheritParams dplyr::count
#' @inheritParams summarise.SpatVector
#'
#' @return A `SpatVector` object with an additional attribute.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::aggregate()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::count()] family functions for
#' `SpatVector` objects.
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
  # Maybe regroup
  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE, .drop = .drop)
  } else {
    out <- x
  }

  vend <- tally(out, sort = sort, name = name)

  # Prepare a template for groups
  template <- dplyr::count(as_tibble(x), ...,
    sort = sort, name = name,
    .drop = .drop
  )


  # Dissolve if requested
  if (.dissolve) {
    keepdf <- as_tibble(vend)

    var_index <- make_safe_index("tterra_index", keepdf)
    vend[[var_index]] <- seq_len(nrow(keepdf))
    vend <- terra::aggregate(vend, by = var_index, dissolve = TRUE)
    vend <- cbind(vend[, 0], keepdf)
  }

  # Ensure groups
  vend <- ungroup(vend)

  # Re-group based on the template
  if (dplyr::is_grouped_df(template)) {
    gvars <- dplyr::group_vars(template)
    vend <- group_by(vend, across_all_of(gvars))
  }


  vend
}

#' @export
dplyr::count

#' @importFrom dplyr tally
#' @export
#' @name count.SpatVector
tally.SpatVector <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  # Use terra method on ungrouped
  if (!is_grouped_spatvector(x)) {
    vargroup <- make_safe_index("tterra_index", x)
    x[[vargroup]] <- "UNIQUE"

    vend <- terra::aggregate(x, by = vargroup, dissolve = FALSE, count = TRUE)
    # Keep aggregation only and rename
    vend <- vend[, "agg_n"]
    if (is.null(name)) name <- "n"

    names(vend) <- name
    return(vend)
  }

  # Get tibble and index of rows
  tblforindex <- as_tibble(x)
  # Get a template
  template <- dplyr::tally(tblforindex, sort = sort, name = name)

  vargroup <- dplyr::group_vars(tblforindex)
  x <- x[, vargroup]
  vend <- terra::aggregate(x, by = vargroup, dissolve = FALSE, count = TRUE)
  # Keep and rename
  vend <- vend[, c(vargroup, "agg_n")]

  if (sort) {
    # Re-sort
    vend <- vend[order(vend$agg_n, decreasing = TRUE), ]
  }
  names(vend) <- names(template)
  vend <- ungroup(vend)


  # Re-group based on the template
  if (dplyr::is_grouped_df(template)) {
    gvars <- dplyr::group_vars(template)
    vend <- group_by(vend, across_all_of(gvars))
  }

  vend
}



#' @export
dplyr::tally
