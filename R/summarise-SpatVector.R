#' Summarise each group of a `SpatVector` down to one geometry
#'
#' @description
#' `summarise()` creates a new `SpatVector`. It returns one geometry for each
#' combination of grouping variables; if there are no grouping variables, the
#' output will have a single geometry summarising all observations in the input
#' and combining all the geometries of the `SpatVector`. It will contain one
#' column for each grouping variable and one column for each of
#' the summary statistics that you have specified.
#'
#' `summarise.SpatVector()` and `summarize.SpatVector()` are synonyms
#'
#' @return
#' A `SpatVector`.
#'
#' @export
#' @rdname summarise.SpatVector
#' @name summarise.SpatVector
#'
#' @seealso [dplyr::summarise()], [terra::aggregate()]
#'
#' @family single table verbs
#' @family dplyr.groups
#' @family dplyr.methods
#'
#' @param .data A `SpatVector`.
#'
#' @inheritParams dplyr::summarise
#' @param .dissolve logical. Should borders between aggregated geometries
#'   be dissolved?
#'
#' @importFrom dplyr summarise
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::aggregate()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::summarise()] function.
#'
#' ## `SpatVector`
#'
#' Similarly to the implementation on \CRANpkg{sf} this function can be used to
#' dissolve geometries (with `.dissolve = TRUE`) or create `MULTI` versions of
#' geometries (with `.dissolve = FALSE`). See **Examples**.
#'
#' @examples
#' library(terra)
#' library(ggplot2)
#'
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' # Grouped
#' gr_v <- v |>
#'   mutate(start_with_s = substr(name, 1, 1) == "S") |>
#'   group_by(start_with_s)
#'
#' # Dissolving
#' diss <- gr_v |>
#'   summarise(n = dplyr::n(), mean = mean(as.double(cpro)))
#'
#' diss
#'
#' autoplot(diss, aes(fill = start_with_s)) + ggplot2::ggtitle("Dissolved")
#'
#' # Not dissolving
#' no_diss <- gr_v |>
#'   summarise(n = dplyr::n(), mean = mean(as.double(cpro)), .dissolve = FALSE)
#'
#' # Same statistic
#' no_diss
#'
#' autoplot(no_diss, aes(fill = start_with_s)) +
#'   ggplot2::ggtitle("Not Dissolved")
summarise.SpatVector <- function(
  .data,
  ...,
  .by = NULL,
  .groups = NULL,
  .dissolve = TRUE
) {
  # Try find .by vectors
  by_groups <- group_by(.data, {{ .by }})

  # Get dfs
  df <- as_tibble(.data)
  df_summ <- dplyr::summarise(df, ..., .groups = .groups, .by = {{ .by }})

  spatv <- .data

  if (is_grouped_spatvector(spatv)) {
    spatv$tterra_index <- group_indices(df)
    newgeom <- terra::aggregate(
      spatv,
      by = "tterra_index",
      dissolve = .dissolve
    )
  } else if (is_rowwise_spatvector(spatv)) {
    # Do nothing, rowwise respect rows
    newgeom <- spatv
  } else if (is_grouped_spatvector(by_groups)) {
    spatv$tterra_index <- group_indices(by_groups)
    newgeom <- terra::aggregate(
      spatv,
      by = "tterra_index",
      dissolve = .dissolve
    )
  } else {
    newgeom <- terra::aggregate(spatv, dissolve = .dissolve)
  }

  v_summ <- cbind(newgeom[, 0], df_summ)

  # Ensure groups
  v_summ <- group_prepare_spat(v_summ, df_summ)

  v_summ
}

#' @export
dplyr::summarise

#' @rdname summarise.SpatVector
#' @importFrom dplyr summarize
#' @export
summarize.SpatVector <- summarise.SpatVector

#' @export
dplyr::summarize
