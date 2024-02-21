#' Pivot `SpatVector` from long to wide
#
#' @description
#' [pivot_wider()] "widens" a `SpatVector`, increasing the number of columns and
#' decreasing the number of rows. The inverse transformation is
#' [pivot_longer.SpatVector()].
#'
#' @export
#' @importFrom tidyr pivot_wider
#'
#' @family tidyr.methods
#'
#' @rdname pivot_wider.SpatVector
#' @name pivot_wider.SpatVector
#'
#'
#' @param id_cols <[`tidy-select`][tidyr::tidyr_tidy_select]> A set of columns
#'   that uniquely identify each observation. Typically used when you have
#'   redundant variables, i.e. variables whose values are perfectly correlated
#'   with existing variables.
#'
#'   Defaults to all columns in `data` except for the columns specified through
#'   `names_from` and `values_from`. If a
#'   [`tidyselect`][tidyr::tidyr_tidy_select] expression is supplied, it
#'   will be evaluated on `data` after removing the columns specified through
#'   `names_from` and `values_from`.
#'
#'   Note that "`geometry`" columns is sticky, hence it would be
#'   removed from `names_from` and `values_from`.
#'
#' @inheritParams pivot_longer.SpatVector
#' @inheritParams tidyr::pivot_wider
#'
#' @return A SpatVector object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::pivot_wider()] function.
#'
#' ## SpatVector
#'
#' The geometry column has a sticky behavior. This means that the result would
#' have always the geometry of `data`.
#'
#' @examples
#'
#'
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))
#'
#' names(nc)
#' # Pivot longer
#' nc_long <- nc %>%
#'   pivot_longer(contains("BIR"), names_to = "label", values_to = "n") %>%
#'   select(label:n) %>%
#'   glimpse()
#'
#'
#' # Undo
#' nc_long %>%
#'   pivot_wider(names_from = label, values_from = n) %>%
#'   glimpse()
#'
pivot_wider.SpatVector <- function(data,
                                   ...,
                                   id_cols = NULL,
                                   id_expand = FALSE,
                                   names_from = name,
                                   names_prefix = "",
                                   names_sep = "_",
                                   names_glue = NULL,
                                   names_sort = FALSE,
                                   names_vary = "fastest",
                                   names_expand = FALSE,
                                   names_repair = "check_unique",
                                   values_from = value,
                                   values_fill = NULL,
                                   values_fn = NULL,
                                   unused_fn = NULL) {
  # as tibble with attrbs
  tbl <- as_tbl_internal(data)
  att <- attributes(tbl)


  # Handle col names as character vectors ----
  tmpl <- dplyr::ungroup(tbl[1, ])
  nmfrom <- names(dplyr::select(tmpl, {{ names_from }}))

  if ("geometry" %in% nmfrom) {
    cli::cli_alert_warning(
      "Ommiting {.val geometry} column from {.arg names_from} argument."
    )

    nmfrom <- setdiff(nmfrom, "geometry")
  }

  vfrom <- names(dplyr::select(tmpl, {{ values_from }}))

  if ("geometry" %in% vfrom) {
    cli::cli_alert_warning(
      "Ommiting {.val geometry} column from {.arg values_from} argument."
    )

    vfrom <- setdiff(vfrom, "geometry")
  }

  pivoted <- tidyr::pivot_wider(tbl,
    ...,
    id_cols = {{ id_cols }},
    id_expand = id_expand,
    names_from = dplyr::all_of(nmfrom),
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = names_glue,
    names_sort = names_sort,
    names_vary = names_vary,
    names_expand = names_expand,
    names_repair = names_repair,
    values_from = dplyr::all_of(vfrom),
    values_fill = values_fill,
    values_fn = values_fn,
    unused_fn = unused_fn
  )

  # nocov start
  if (!"geometry" %in% names(pivoted)) {
    cli::cli_abort(
      paste0(
        "Can't rebuild the {.cls SpatVector}, ",
        "{.val geometry} column lost after pivoting"
      )
    )
  }
  # nocov end

  # Reconstruct table
  attr(pivoted, "source") <- att$source
  attr(pivoted, "crs") <- att$crs
  attr(pivoted, "geomtype") <- att$geomtype

  sv <- as_spat_internal(pivoted)

  return(sv)
}
