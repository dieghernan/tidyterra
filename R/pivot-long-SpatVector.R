#' Pivot `SpatVector` from wide to long
#'
#' @description
#' `pivot_longer()` "lengthens" data, increasing the number of rows and
#' decreasing the number of columns. The inverse transformation is
#' [pivot_wider.SpatVector()]
#'
#' Learn more in [tidyr::pivot_longer()].
#'
#' @export
#' @encoding UTF-8
#' @rdname pivot_longer.SpatVector
#' @name pivot_longer.SpatVector
#'
#' @seealso [tidyr::pivot_longer()]
#'
#' @family tidyr.pivot
#' @family tidyr.methods
#'
#' @importFrom tidyr pivot_longer
#'
#' @inheritParams tidyr::pivot_longer
#'
#' @param data A `SpatVector` to pivot.
#'
#' @returns A `SpatVector` object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::pivot_longer()] method.
#'
#' ## `SpatVector`
#'
#' The geometry column has sticky behavior. This means that the result always
#' has the geometry of `data`.
#'
#' @examples
#' \donttest{
#'
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' library(terra)
#'
#' temp <- rast((system.file("extdata/cyl_temp.tif", package = "tidyterra")))
#' cyl <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra")) |>
#'   project(temp)
#'
#' # Add average temp
#'
#' temps <- terra::extract(temp, cyl, fun = "mean", na.rm = TRUE, xy = TRUE)
#' cyl_temp <- cbind(cyl, temps) |>
#'   glimpse()
#'
#' # And pivot long for plot
#' cyl_temp |>
#'   pivot_longer(
#'     cols = tavg_04:tavg_06,
#'     names_to = "label",
#'     values_to = "temp"
#'   ) |>
#'   ggplot() +
#'   geom_spatvector(aes(fill = temp)) +
#'   facet_wrap(~label, ncol = 1) +
#'   scale_fill_whitebox_c(palette = "muted")
#' }
pivot_longer.SpatVector <- function(
  data,
  cols,
  ...,
  cols_vary = "fastest",
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = NULL,
  names_transform = NULL,
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes = NULL,
  values_transform = NULL
) {
  # Convert to a tibble with attributes.
  tbl <- as_tbl_internal(data)

  att <- attributes(tbl)

  # Resolve columns from a template.
  tmpl <- dplyr::ungroup(tbl[1, ])
  cols_char <- remove_geom_col(tmpl, {{ cols }}, "cols")

  pivoted <- tidyr::pivot_longer(
    data = tbl,
    cols = dplyr::all_of(cols_char),
    ...,
    cols_vary = cols_vary,
    names_to = names_to,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_pattern = names_pattern,
    names_ptypes = names_ptypes,
    names_transform = names_transform,
    names_repair = names_repair,
    values_to = values_to,
    values_drop_na = values_drop_na,
    values_ptypes = values_ptypes,
    values_transform = values_transform
  )
  if (!"geometry" %in% names(pivoted)) {
    abort_lost_geometry_after_pivot()
  }

  # Reconstruct the table.
  attr(pivoted, "source") <- att$source
  attr(pivoted, "crs") <- att$crs
  attr(pivoted, "geomtype") <- att$geomtype

  sv <- as_spat_internal(pivoted)

  sv
}

#' @export
tidyr::pivot_longer

# Safely remove `geometry` from a tidyselect expression.
# Return a character vector.
remove_geom_col <- function(data, exp, var_name = "any") {
  nm <- dplyr::select(data, {{ exp }})

  sel_names <- names(nm)
  if ("geometry" %in% sel_names) {
    cli::cli_alert_warning(
      "Omitting {.val geometry} column from {.arg {var_name}} argument."
    )

    sel_names <- setdiff(sel_names, "geometry")
  }

  sel_names
}
