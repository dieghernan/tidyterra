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
#' r <- rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")
#'
#' r
#'
#' # Create tibble
#' as_tbl <- as_tibble(r, xy = TRUE)
#'
#' as_tbl
#'
#' # From tibble
#' newrast <- as_spatraster(as_tbl, crs = "EPSG:3857")
#' newrast
#'
as_spatraster <- function(x, ..., xycols = 1:2, crs = "", digits = 6) {
  if (inherits(x, "SpatRaster")) {
    return(x)
  }

  # Create from dtplyr
  if (inherits(x, "dtplyr_step")) x <- tibble::as_tibble(x)

  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      "{.arg x} should be a {.cls data.frame/tbl}, not {.cls {class(x)}}"
    )
  }

  if (length(xycols) != 2) {
    cli::cli_abort(paste(
      "{.arg xycols} should have a length of {.val {as.integer(2)}},",
      "not {.val {length(xycols)}}"
    ))
  }

  if (!is.numeric(xycols)) {
    cli::cli_abort(
      "{.arg xycols} should be a {.cls integer}, not {.cls {class(xycols)}}"
    )
  }

  xycols <- as.integer(xycols)

  # To tibble
  x <- tibble::as_tibble(x)

  # Rearrange cols
  xy_cols <- x[xycols]
  values <- x[-xycols]

  names(xy_cols) <- c("x", "y")
  layer_names <- names(values)

  # Check if grid is regular
  is_regular_grid(xy_cols, digits = digits)

  names(values) <- paste0("lyr", seq_len(ncol(values)))

  x_arrange <- dplyr::bind_cols(xy_cols, values)

  # Create SpatRaster
  # crs
  crs_attr <- attr(x, "crs")
  crs <- pull_crs(crs)

  # Check from attrs
  if (is.na(crs)) crs <- crs_attr

  if (is.na(pull_crs(crs))) crs <- NA


  # Issue: work with layer/columns with NA
  # Check class of columns
  col_classes <- unlist(lapply(values, is.numeric))

  # If all are numeric happy days!
  if (all(col_classes)) {
    newrast <- terra::rast(x_arrange,
      crs = crs, ..., type = "xyz",
      digits = digits
    )

    names(newrast) <- layer_names
    return(newrast)
  }

  # If not, different strategy:
  # a. Create a template raster with index for values
  # b. Extract full grid and attach values
  # c. Add values to template grid

  # xyvalues plus index

  xyvalind <- x_arrange[, 1:2]
  xyvalind$valindex <- seq_len(nrow(xyvalind))


  values_w_ind <- x_arrange[, -c(1, 2)]
  values_w_ind$valindex <- xyvalind$valindex


  # Create template

  r_temp <- terra::rast(xyvalind,
    crs = crs, ..., type = "xyz",
    digits = digits
  )

  # Expand grid
  r_temp_df <- terra::as.data.frame(r_temp, na.rm = FALSE, xy = FALSE)
  r_temp_df <- tibble::as_tibble(r_temp_df)

  r_temp_df <- dplyr::left_join(r_temp_df, values_w_ind, by = "valindex")

  values <- r_temp_df[, -1]

  # Now assign values to raster
  terra::nlyr(r_temp) <- 1

  nlyrs <- ncol(values)

  temp_list <- lapply(seq_len(nlyrs), function(x) {
    terra::setValues(
      r_temp,
      unlist(values[, x])
    )
  })

  # Finally unlist rasters and fix names
  defortify <- do.call("c", temp_list)

  names(defortify) <- layer_names

  return(defortify)
}

#' Rebuild objects created with as_tbl_spatattr to SpatRaster
#' Strict version, used attributes for creating a template
#' SpatRaster and then transfer the values
#' @noRd
as_spatrast_attr <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(x)
  }

  # Create from dtplyr
  x <- data.table::as.data.table(x)

  # Get attributes
  attrs <- attributes(x)

  # Get number of layers
  values <- dplyr::select(x, -c(1, 2))
  values <- data.table::as.data.table(values)


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
