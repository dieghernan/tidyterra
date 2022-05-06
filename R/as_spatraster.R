#' Coerce a data frame to SpatRaster
#'
#' @description
#'
#' `as_spatraster()` turns an existing  data frame or tibble, into a SpatRaster.
#' This is a wrapper of [terra::rast()] S4 method for `data.frame`.
#'
#' @return
#' A SpatRaster.
#'
#' @export
#'
#' @param x A tibble or data frame.
#' @param xycols A vector of integers of length 2 determining the position of
#'   the columns that hold the x and y coordinates.
#' @param digits integer to set the precision for detecting whether points are
#'   on a regular grid (a low number of digits is a low precision).
#'
#' @param crs A crs on several formats (PROJ.4, WKT, EPSG code, ..) or
#'   and spatial object from sf or terra that includes the target coordinate
#'   reference system. See [pull_crs()]. See **Details**.
#'
#' @param ... additional arguments passed on to [terra::rast()].
#'
#' @details
#'
#' `r lifecycle::badge('questioning')` If no `crs` is provided and the tibble
#' has been created with the method [as_tibble.SpatRaster()], the `crs` is
#' inferred from `attr(x, "crs")`.
#'
#' @family coerce
#'
#' @seealso [pull_crs()]
#'
#' @section terra equivalent:
#'
#' [terra::rast()]
#'
#' @examples
#' library(terra)
#'
#' r <- rast(matrix(1:90, ncol = 3), crs = "epsg:3857")
#'
#' r
#'
#' # Create tibble
#' as_tbl <- as_tibble(r, xy = TRUE)
#'
#' as_tbl
#'
#' # From tibble
#' newrast <- as_spatraster(as_tbl, crs = "epsg:3857")
#' newrast
#'
as_spatraster <- function(x, ..., xycols = 1:2, crs = "", digits = 6) {
  if (inherits(x, "SpatRaster")) {
    return(x)
  }

  if (length(xycols) != 2 | !is.numeric(xycols)) {
    stop("xycols should be two integers: `c(int, int)`")
  }

  xycols <- as.integer(xycols)

  # To tibble
  x <- tibble::as_tibble(x)

  # Rearrange cols
  xy_cols <- x[xycols]
  values <- x[-xycols]

  x_arrange <- dplyr::bind_cols(xy_cols, values)

  # Check if grid is regular
  is_regular_grid(x_arrange, digits = digits)

  # Create SpatRaster
  x_arrange <- as.data.frame(x_arrange)

  # crs
  crs_attr <- attr(x, "crs")
  crs <- pull_crs(crs)

  # Check from attrs
  if (is.na(crs)) crs <- crs_attr

  if (is.na(pull_crs(crs))) crs <- NA

  newrast <- terra::rast(x_arrange,
    crs = crs, ..., type = "xyz",
    digits = digits
  )

  return(newrast)
}

#' Rebuild objects created with as_tbl_spatattr to SpatRaster
#' Strict version, used attributes for creating a template
#' SpatRaster and then transfer the values
#' @noRd
as_spatrast_attr <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(x)
  }

  if (!isTRUE((attr(x, "source")) == "tbl_terra_spatraster")) {
    cli::cli_alert_danger(
      paste(
        "Spatial attributes may have been removed.",
        "Check the result carefully."
      )
    )
    return(as_spatraster(x))
  }


  # Get attributes
  attrs <- attributes(x)

  # Get number of layers
  values <- x[, -c(1:2)]

  nlyrs <- ncol(values)

  # Create a list of rasters for each layer
  # and assign value


  temp_list <- lapply(seq_len(nlyrs), function(x) {
    terra::setValues(
      terra::rast(
        nrows = attrs$dims[1],
        ncols = attrs$dims[2],
        nlyrs = 1,
        crs = attrs$crs,
        extent = attrs$ext,
        resolution = attrs$res
      ),
      unlist(values[, x])
    )
  })

  # Finally unlist rasters and fix name
  defortify <- do.call("c", temp_list)
  names(defortify) <- names(values)

  return(defortify)
}
