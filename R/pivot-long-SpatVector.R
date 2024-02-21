#' Pivot SpatVector from wide to long
#'
#' @description
#' `pivot_longer()` "lengthens" data, increasing the number of rows and
#' decreasing the number of columns. The inverse transformation is
#' [tidyr::pivot_wider()]
#'
#' Learn more in [tidyr::pivot_wider()].
#'
#' @export
#' @importFrom tidyr pivot_longer
#'
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
#' @return A SpatVector object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::pivot_longer()] function.
#'
#' ## SpatVector
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
#' # Usually this is useful for facetting, see BIR
#' nc_pivoted <- nc %>%
#'   pivot_longer(starts_with("BIR"),
#'     names_to = "year",
#'     values_to = "births"
#'   ) %>%
#'   mutate(year = as.numeric(gsub("BIR", "19", year))) %>%
#'   select(year, births)
#'
#' ggplot(nc_pivoted) +
#'   geom_spatvector(aes(fill = births)) +
#'   facet_wrap(~year, ncol = 1) +
#'   scale_fill_viridis_c(option = "cividis") +
#'   labs(title = "Number of births in South Carolina")
#'
pivot_longer.SpatVector <- function(data, cols, ..., cols_vary = "fastest",
                                    names_to = "name", names_prefix = NULL,
                                    names_sep = NULL, names_pattern = NULL,
                                    names_ptypes = NULL, names_transform = NULL,
                                    names_repair = "check_unique",
                                    values_to = "value", values_drop_na = FALSE,
                                    values_ptypes = NULL,
                                    values_transform = NULL) {
  # Add an index
  data$tterrageom <- seq_len(nrow(data))

  # Pivot with tibble
  tbl <- as_tibble(data)

  # Keep geom
  data <- ungroup(data)
  geom_only <- as_tbl_internal(data["tterrageom"])




  pivoted <- tidyr::pivot_longer(
    data = tbl, cols = {{ cols }}, ...,
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

  # Remove geometry if produced
  pivoted <- pivoted[setdiff(names(pivoted), "geometry")]

  if (!"tterrageom" %in% names(pivoted)) {
    cli::cli_abort(
      "Can't pivot {.val geometry} of the {.cls SpatVector} with these args"
    )
  }

  # Reconstruct table
  sv_tab <- dplyr::left_join(pivoted, geom_only, by = "tterrageom")

  # Remove the tterra_index
  sv_tab <- sv_tab[setdiff(names(sv_tab), "tterrageom")]

  # Reconvert safely

  attr(sv_tab, "crs") <- pull_crs(data)
  attr(sv_tab, "geomtype") <- terra::geomtype(data)
  attr(sv_tab, "source") <- "SpatVector"

  sv_end <- as_spat_internal(sv_tab)

  return(sv_end)
}
