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
replace_na.SpatRaster <- function(data, replace = list(), ...) {

  # Create template matrix
  # Don't need conversion to data.frame
  df <- data[1]
  # Convert factors to chars
  is_factor <- sapply(df, is.factor)
  df[is_factor] <- lapply(df[is_factor], as.character)

  raster_names <- names(df)

  # Set NAs
  df[1, ] <- NA

  # Replace NA
  df_na <- tidyr::replace_na(df, replace = replace, ...)

  # Check the columns that have changed
  if (all(is.na(df_na[1, ]))) {
    # Nothing changed, return the spatraster
    return(data)
  }

  # Get the index of changed layers
  check_index <- as.logical(!is.na(df_na[1, ]))
  index_cols <- seq_len(terra::nlyr(data))[check_index]

  # Replace on new raster
  newrast <- data

  for (i in index_cols) {
    # Values to replace
    vals <- terra::as.data.frame(newrast[[i]], na.rm = FALSE)
    vals <- unlist(vals)

    is.factor(vals)

    if (is.factor(vals)) vals <- as.character(vals)

    new_val <- unlist(df_na[1, i])[1]
    vals[is.na(vals)] <- new_val

    terra::values(newrast[[i]]) <- vals
  }

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
