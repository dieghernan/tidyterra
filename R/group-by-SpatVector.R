#' Group a SpatVector by one or more variables
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Most data operations are done on groups defined by variables. [group_by()]
#' adds new attributes to an existing SpatVector indicating the
#' corresponding groups. See **Methods**.
#'
#' @export
#' @rdname group-by.SpatVector
#' @name group-by.SpatVector
#'
#' @seealso [dplyr::group_by()], [dplyr::ungroup()]
#'
#' @family dplyr.groups
#' @family dplyr.methods
#'
#' @importFrom dplyr group_by
#' @param .data,x A SpatVector object. See **Methods**.
#' @inheritParams dplyr::group_by
#'
#' @return A SpatVector object with an additional attribute.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_by()] family functions for
#' SpatVectors.
#'
#' **When mixing** \pkg{terra} **and** \pkg{dplyr} **syntax** on a grouped
#' SpatVector (i.e, subsetting a SpatVector like `v[1:3,1:2]`) the `groups`
#' attribute can be corrupted. \pkg{tidyterra} would try to re-group the
#' SpatVector. This would be triggered the next time you use a dplyr verb on
#' your SpatVector.
#'
#' Note also that some operations (as `terra::spatSample()`) would create a new
#' SpatVector. In these cases, the result won't preserve the `groups` attribute.
#' Use [group_by.SpatVector()] to re-group.
#'
#' @details
#'
#' See **Details** on [dplyr::group_by()].
#'
#' @examples
#' \donttest{
#'
#' library(terra)
#' f <- system.file("ex/lux.shp", package = "terra")
#' p <- vect(f)
#'
#'
#' by_name1 <- p %>% group_by(NAME_1)
#'
#' # grouping doesn't change how the SpatVector looks
#' by_name1
#'
#' # But add metadata for grouping: See the coercion to tibble
#'
#' # Not grouped
#' p_tbl <- as_tibble(p)
#' class(p_tbl)
#' head(p_tbl, 3)
#'
#' # Grouped
#' by_name1_tbl <- as_tibble(by_name1)
#' class(by_name1_tbl)
#' head(by_name1_tbl, 3)
#'
#'
#' # It changes how it acts with the other dplyr verbs:
#' by_name1 %>% summarise(
#'   pop = mean(POP),
#'   area = sum(AREA)
#' )
#'
#' # Each call to summarise() removes a layer of grouping
#' by_name2_name1 <- p %>% group_by(NAME_2, NAME_1)
#'
#' by_name2_name1
#' group_data(by_name2_name1)
#'
#' by_name2 <- by_name2_name1 %>% summarise(n = dplyr::n())
#' by_name2
#' group_data(by_name2)
#'
#' # To removing grouping, use ungroup
#' by_name2 %>%
#'   ungroup() %>%
#'   summarise(n = sum(n))
#'
#' # By default, group_by() overrides existing grouping
#' by_name2_name1 %>%
#'   group_by(ID_1, ID_2) %>%
#'   group_vars()
#'
#'
#' # Use add = TRUE to instead append
#' by_name2_name1 %>%
#'   group_by(ID_1, ID_2, .add = TRUE) %>%
#'   group_vars()
#'
#' # You can group by expressions: this is a short-hand
#' # for a mutate() followed by a group_by()
#' p %>%
#'   group_by(ID_COMB = ID_1 * 100 / ID_2) %>%
#'   relocate(ID_COMB, .before = 1)
#' }
group_by.SpatVector <- function(.data, ..., .add = FALSE,
                                .drop = group_by_drop_default(.data)) {
  # Use own method
  x <- .data

  .data <- as_tibble(.data)

  # Add groups
  newgroups <- dplyr::group_by(.data, ..., .add = .add, .drop = .drop)

  regen <- cbind(x[, 0], newgroups)

  # Add groups metadata
  regen <- group_prepare_spat(regen, newgroups)

  regen
}

#' @export
dplyr::group_by

#' @importFrom dplyr ungroup
#' @export
#' @name group-by.SpatVector
ungroup.SpatVector <- function(x, ...) {
  # Use own method
  if (!is_grouped_spatvector(x)) {
    return(x)
  }

  # Template
  g_tbl <- as_tibble(x)
  # Ungroup default method
  newgroups <- dplyr::ungroup(g_tbl, ...)


  # Add groups metadata
  x <- group_prepare_spat(x, newgroups)

  return(x)
}

#' @export
dplyr::ungroup

#' @importFrom dplyr group_by_drop_default
#' @export
dplyr::group_by_drop_default

# Internal
# Assign groups to a SpatVector given the info of a template
group_prepare_spat <- function(x, template) {
  # x not SpatVector
  if (!inherits(x, "SpatVector")) {
    return(x)
  }

  # template is not df
  # Gives an error
  # nocov start
  if (!inherits(template, "data.frame")) stop("Using bad grouping template")
  # nocov end

  if (inherits(template, "grouped_df")) {
    attr(x, "tblclass") <- "grouped_df"
    attr(x, "groups") <- attr(template, "groups")
  } else {
    # Ungroup
    attr(x, "tblclass") <- NULL
    attr(x, "groups") <- NULL
  }
  return(x)
}
