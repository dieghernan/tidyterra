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
#' @family tidyr.pivot
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
#' @seealso [tidyr::pivot_wider()]
#'
#' @return A `SpatVector` object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::pivot_wider()] function.
#'
#' ## `SpatVector`
#'
#' The geometry column has a sticky behavior. This means that the result would
#' have always the geometry of `data`.
#'
#' @examples
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
                                   names_from = "name",
                                   names_prefix = "",
                                   names_sep = "_",
                                   names_glue = NULL,
                                   names_sort = FALSE,
                                   names_vary = "fastest",
                                   names_expand = FALSE,
                                   names_repair = "check_unique",
                                   values_from = "value",
                                   values_fill = NULL,
                                   values_fn = NULL,
                                   unused_fn = NULL) {
  # as tibble with attrbs
  tbl <- as_tbl_internal(data)
  att <- attributes(tbl)


  # Intercept cols using a template
  tmpl <- dplyr::ungroup(tbl[1, ])
  names_from_char <- remove_geom_col(tmpl, {{ names_from }}, "names_from")
  values_from_char <- remove_geom_col(tmpl, {{ values_from }}, "values_from")
  id_cols_char <- tt_sel_wider_id_cols(
    tmpl, {{ id_cols }},
    names_from_char, values_from_char
  )

  pivoted <- tidyr::pivot_wider(tbl,
    ...,
    id_cols = dplyr::all_of(id_cols_char), id_expand = id_expand,
    names_from = dplyr::all_of(names_from_char), names_prefix = names_prefix,
    names_sep = names_sep, names_glue = names_glue, names_sort = names_sort,
    names_vary = names_vary, names_expand = names_expand,
    names_repair = names_repair, values_from = dplyr::all_of(values_from_char),
    values_fill = values_fill, values_fn = values_fn, unused_fn = unused_fn
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

#' @export
tidyr::pivot_wider

# Based on tidyr:::select_wider_id_cols
# Retuns always a character vector
tt_sel_wider_id_cols <- function(data,
                                 id_cols = NULL,
                                 names_from_cols = character(),
                                 values_from_cols = character()) {
  id_cols_quo <- rlang::enquo(id_cols)

  # Remove known non-id-cols so they are never selected
  data <- data[setdiff(names(data), c(names_from_cols, values_from_cols))]


  if (rlang::quo_is_null(id_cols_quo)) {
    # Default selects everything in `data` after non-id-cols have been removed
    idnm <- names(data)
  } else {
    idnm <- names(dplyr::select(data, {{ id_cols }}))
  }
  # geometry is top-level var always
  idnm <- unique(c("geometry", idnm))

  idnm
}
