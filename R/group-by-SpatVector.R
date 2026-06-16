#' Group a `SpatVector` by one or more variables
#'
#' @description
#'
#' Most data operations are done on groups defined by variables.
#' [group_by.SpatVector()] adds new attributes to an existing `SpatVector`
#' indicating the corresponding groups. See **Methods**.
#'
#' @details
#'
#' See **Details** on [dplyr::group_by()].
#'
#' @export
#' @encoding UTF-8
#' @rdname group_by.SpatVector
#' @name group_by.SpatVector
#'
#' @seealso [dplyr::group_by()], [dplyr::ungroup()]
#'
#' @family dplyr.groups
#' @family dplyr.methods
#' @family dplyr.group_functions
#'
#' @importFrom dplyr group_by
#' @inheritParams dplyr::group_by
#'
#' @param .data,x A `SpatVector` object. See **Methods**.
#' @returns A `SpatVector` object with updated grouping metadata.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_by()] family functions for
#' `SpatVector` objects.
#'
#' **When mixing** \CRANpkg{terra} **and** \CRANpkg{dplyr} **syntax**
#' on a grouped `SpatVector`, for example subsetting a `SpatVector` like
#' `v[1:3,1:2]`, the `groups` attribute can be corrupted.
#' \CRANpkg{tidyterra} tries to re-group the `SpatVector`. This is triggered
#' the next time you use a \CRANpkg{dplyr} verb on your `SpatVector`.
#'
#' Some operations, such as `terra::spatSample()`, create a new `SpatVector`.
#' In these cases, the result does not preserve the `groups` attribute. Use
#' [group_by()] to re-group.
#'
#' @examples
#' \donttest{
#'
#' library(terra)
#' f <- system.file("ex/lux.shp", package = "terra")
#' p <- vect(f)
#'
#' by_name1 <- p |> group_by(NAME_1)
#'
#' # Grouping does not change how the SpatVector looks.
#' by_name1
#'
#' # But it adds metadata for grouping. See the coercion to tibble.
#'
#' # Not grouped.
#' p_tbl <- as_tibble(p)
#' class(p_tbl)
#' head(p_tbl, 3)
#'
#' # Grouped.
#' by_name1_tbl <- as_tibble(by_name1)
#' class(by_name1_tbl)
#' head(by_name1_tbl, 3)
#'
#' # It changes how it acts with the other dplyr verbs:
#' by_name1 |> summarise(
#'   pop = mean(POP),
#'   area = sum(AREA)
#' )
#'
#' # Each call to summarise() removes a layer of grouping.
#' by_name2_name1 <- p |> group_by(NAME_2, NAME_1)
#'
#' by_name2_name1
#' group_data(by_name2_name1)
#'
#' by_name2 <- by_name2_name1 |> summarise(n = dplyr::n())
#' by_name2
#' group_data(by_name2)
#'
#' # To remove grouping, use ungroup().
#' by_name2 |>
#'   ungroup() |>
#'   summarise(n = sum(n))
#'
#' # By default, group_by() overrides existing grouping.
#' by_name2_name1 |>
#'   group_by(ID_1, ID_2) |>
#'   group_vars()
#'
#' # Use add = TRUE to append instead.
#' by_name2_name1 |>
#'   group_by(ID_1, ID_2, .add = TRUE) |>
#'   group_vars()
#'
#' # You can group by expressions. This is shorthand for a mutate() followed
#' # by a group_by().
#' p |>
#'   group_by(ID_COMB = ID_1 * 100 / ID_2) |>
#'   relocate(ID_COMB, .before = 1)
#' }
group_by.SpatVector <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = group_by_drop_default(.data)
) {
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

#' @export
#' @encoding UTF-8
#' @name group_by.SpatVector
#' @importFrom dplyr ungroup
ungroup.SpatVector <- function(x, ...) {
  # Use own method
  getattr <- attr(x, "tblclass")
  if (is.null(getattr) || !getattr %in% c("grouped_df", "rowwise_df")) {
    return(x)
  }

  # Template
  g_tbl <- as_tibble(x)
  # Ungroup default method
  newgroups <- dplyr::ungroup(g_tbl, ...)

  # Add groups metadata
  x <- group_prepare_spat(x, newgroups)

  x
}

#' @export
dplyr::ungroup

#' @export
#' @encoding UTF-8
#' @importFrom dplyr group_by_drop_default
dplyr::group_by_drop_default

# Internal
# Assign groups to a `SpatVector` using template metadata.
group_prepare_spat <- function(x, template) {
  # Check that `x` is a `SpatVector`.
  if (!inherits(x, "SpatVector")) {
    return(x)
  }

  # Check that the template is a data frame.
  # Gives an error
  if (!inherits(template, "data.frame")) {
    cli::cli_abort("The grouping template must be a data frame.")
  }

  if (dplyr::is_grouped_df(template)) {
    attr(x, "tblclass") <- "grouped_df"
    attr(x, "groups") <- attr(template, "groups")
  } else if (is_rowwise_df(template)) {
    attr(x, "tblclass") <- "rowwise_df"
    attr(x, "groups") <- attr(template, "groups")
  } else {
    # Ungroup
    attr(x, "tblclass") <- NULL
    attr(x, "groups") <- NULL
  }
  x
}
