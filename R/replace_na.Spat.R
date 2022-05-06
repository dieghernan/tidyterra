#' Replace NAs with specified values
#'
#' @description
#'
#' Replace `NAs` on layers/attributes with specified values
#'
#'
#' @inheritParams drop_na
#' @param replace list of values, with one value for each layer/attribute that
#'   has `NA` values to be replaced.
#' @param ... Ignored
#'
#' @return A Spat* object  of the same class than `data`. Geometries and
#'   spatial attributes are preserved.
#'
#' @export
#' @rdname replace_na
#' @name replace_na
#' @importFrom tidyr replace_na
#'
#' @seealso [tidyr::replace_na()]
#'
#' @family tidyr.methods
#'
#' @section  terra equivalent:
#'
#' Use `r[is.na(r)] <- <replacement>`
#'
#' @examples
#'
#' library(terra)
#'
#' f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#' r <- rast(f)
#'
#' r %>% plot()
#'
#' r %>%
#'   replace_na(list(tavg_04 = 6, tavg_06 = 20)) %>%
#'   plot()
#'
replace_na.SpatRaster <- function(data, replace, ...) {
  df <- as_tbl_spat_attr(data)
  # Replace NA
  df_na <- tidyr::replace_na(df, replace = replace, ...)

  # Rebuild newrast
  newrast <- as_spatrast_attr(df_na)

  return(newrast)
}

#' @export
#' @rdname replace_na
replace_na.SpatVector <- function(data, replace, ...) {

  # Use sf method
  sf_obj <- sf::st_as_sf(data)
  replaced <- tidyr::replace_na(sf_obj, replace = replace, ...)

  return(terra::vect(replaced))
}
