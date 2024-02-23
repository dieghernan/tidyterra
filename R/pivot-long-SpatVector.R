#' Pivot `SpatVector` from wide to long
#'
#' @description
#' `pivot_longer()` "lengthens" data, increasing the number of rows and
#' decreasing the number of columns. The inverse transformation is
#' [pivot_wider.SpatVector()]
#'
#' Learn more in [tidyr::pivot_wider()].
#'
#' @export
#' @importFrom tidyr pivot_longer
#'
#' @family tidyr.pivot
#' @family tidyr.methods
#'
#' @rdname pivot_longer.SpatVector
#' @name pivot_longer.SpatVector
#'
#'
#' @param data A SpatVector to pivot.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> Columns to pivot into
#'   longer format.
#' @inheritParams tidyr::pivot_longer
#'
#' @return A `SpatVector` object.
#'
#' @seealso [tidyr::pivot_longer()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::pivot_longer()] function.
#'
#' ## `SpatVector`
#'
#' The geometry column has a sticky behavior. This means that the result would
#' have always the geometry of `data`.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))
#'
#' # Usually this is useful for faceting, see BIR
#' glimpse(nc)
#'
#' nc_pivoted <- nc %>%
#'   pivot_longer(starts_with("BIR"),
#'     names_to = "year",
#'     values_to = "births"
#'   ) %>%
#'   mutate(year = as.numeric(gsub("BIR", "19", year)))
#'
#' nc_pivoted %>%
#'   relocate(year, births) %>%
#'   glimpse()
#'
#' ggplot(nc_pivoted) +
#'   geom_spatvector(aes(fill = births)) +
#'   facet_wrap(~year, ncol = 1) +
#'   scale_fill_viridis_c(option = "cividis") +
#'   labs(title = "Number of births in South Carolina")
pivot_longer.SpatVector <- function(data, cols, ..., cols_vary = "fastest",
                                    names_to = "name", names_prefix = NULL,
                                    names_sep = NULL, names_pattern = NULL,
                                    names_ptypes = NULL, names_transform = NULL,
                                    names_repair = "check_unique",
                                    values_to = "value", values_drop_na = FALSE,
                                    values_ptypes = NULL,
                                    values_transform = NULL) {
  # as tibble with attrbs
  tbl <- as_tbl_internal(data)

  att <- attributes(tbl)

  # Intercept cols using a template
  tmpl <- dplyr::ungroup(tbl[1, ])
  cols_char <- remove_geom_col(tmpl, {{ cols }}, "cols")

  pivoted <- tidyr::pivot_longer(
    data = tbl, cols = dplyr::all_of(cols_char), ..., cols_vary = cols_vary,
    names_to = names_to, names_prefix = names_prefix, names_sep = names_sep,
    names_pattern = names_pattern, names_ptypes = names_ptypes,
    names_transform = names_transform, names_repair = names_repair,
    values_to = values_to, values_drop_na = values_drop_na,
    values_ptypes = values_ptypes, values_transform = values_transform
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
tidyr::pivot_longer

# Helper for removing safely the "geometry" argument from tidyselect expression
# Returns a vector of characters
remove_geom_col <- function(data, exp, var_name = "any") {
  nm <- dplyr::select(data, {{ exp }})

  sel_names <- names(nm)
  if ("geometry" %in% sel_names) {
    cli::cli_alert_warning(
      "Ommiting {.val geometry} column from {.arg {var_name}} argument."
    )

    sel_names <- setdiff(sel_names, "geometry")
  }

  sel_names
}
