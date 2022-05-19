#' Replace NAs with specified values
#'
#' @description
#'
#' Replace `NAs` on layers/attributes with specified values
#'
#'
#' @param data A SpatRaster created with [terra::rast()] or a SpatVector
#'   created with [terra::vect()].
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

  # If no replace return the same data
  if (length(replace) == 0) {
    return(data)
  }

  # Create a template df for assessing results
  # Use only first cell for speed up
  df <- data[1]

  # Convert factors to chars
  is_factor <- sapply(df, is.factor)
  df[is_factor] <- lapply(df[is_factor], as.character)
  # Set NAs
  df[1, ] <- NA

  # Replace NA
  df_na <- tidyr::replace_na(df, replace = replace, ...)

  # Check changed layers
  # Get the index of changed layers
  check_index <- as.logical(!is.na(df_na[1, ]))

  # Replace on new raster using a loop
  # New raster for init the loop
  newrast <- data


  for (i in seq_len(terra::nlyr(newrast))) {
    if (!check_index[i]) next

    # Modify if false
    layer <- terra::subset(newrast, i)

    # Different replacement based on type of layer
    is_factor <- is.factor(dplyr::pull(layer[1]))


    # Check different if it is factor or not
    if (!is_factor) {
      layer[is.na(layer)] <- df_na[1, i]

      # Assign new values
      terra::values(newrast[[i]]) <- pull(layer, na.rm = FALSE)
    } else {
      # For factors
      values <- pull(layer, na.rm = FALSE)
      keep_levs <- levels(values)
      to_replace <- as.character(df_na[1, i])

      # From
      # https://stackoverflow.com/questions/39126537/replace-na-in-a-factor-column
      # New NA level and change label

      new_vals <- factor(values,
        exclude = NULL,
        levels = c(keep_levs, NA),
        labels = c(keep_levs, to_replace)
      )

      terra::values(newrast[[i]]) <- new_vals
    }
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
