#' Coerce a data frame to `SpatRaster`
#'
#' @description
#'
#' `as_spatraster()` converts a data frame or [tibble][tibble::tbl_df] into a
#' `SpatRaster`. It wraps the [terra::rast()] S4 method for signature
#' `data.frame`.
#'
#' @details
#'
#' If no `crs` is provided and the tibble has been created with the method
#' [as_tibble.SpatRaster()], the `crs` is inferred from
#' [`attr(x, "crs")`][attr()].
#'
#' @export
#' @encoding UTF-8
#'
#' @seealso
#'
#' [pull_crs()] for retrieving CRS and the corresponding utilities
#' [sf::st_crs()] and [terra::crs()].
#'
#' @family coerce
#'
#' @param x A [tibble][tibble::tbl_df] or data frame.
#' @param xycols A vector of integers of length 2 determining the position of
#'   the columns that hold the `x` and `y` coordinates.
#'
#' @param digits Integer to set the precision for detecting whether points are
#'   on a regular grid (a low number of digits is a low precision).
#'
#' @param crs A CRS in several formats (PROJ.4, WKT, EPSG code, etc.) or a
#'   spatial object from [sf][sf::st_crs()] or [terra][terra::crs()] that
#'   includes the target coordinate reference system. See [pull_crs()] and
#'   **Details**.
#'
#' @param ... Additional arguments passed on to [terra::rast()].
#'
#' @returns
#' A `SpatRaster`.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::rast()] (see S4 method for signature `data.frame`).
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

  # Materialize lazy dtplyr input.
  if (inherits(x, "dtplyr_step")) {
    x <- tibble::as_tibble(x) # nocov
  }

  if (!inherits(x, "data.frame")) {
    cli::cli_abort(paste0(
      "{.arg x} must be a {.cls data.frame} or {.cls tbl}, ",
      "not {.cls {class(x)}}."
    ))
  }

  if (length(xycols) != 2) {
    cli::cli_abort(paste(
      "{.arg xycols} must have length {.val {as.integer(2)}},",
      "not {.val {length(xycols)}}."
    ))
  }

  if (!is.numeric(xycols)) {
    cli::cli_abort("{.arg xycols} must be numeric, not {.cls {class(xycols)}}.")
  }

  xycols <- as.integer(xycols)

  # Widen fortified pivoted input.
  if (isTRUE(attr(x, "pvt_fort"))) {
    initcrs <- attr(x, "crs")
    x <- x[, 1:4]

    # Restore layer columns.
    names(x) <- c("x", "y", "name", "value")
    x <- tidyr::pivot_wider(x)

    attr(x, "crs") <- initcrs
  }

  # Work with a tibble.
  x <- tibble::as_tibble(x)

  prepared <- prepare_spatraster_cols(x, xycols)
  xy_cols <- prepared$xy_cols
  values <- prepared$values
  layer_names <- prepared$layer_names

  # Check that points form a regular grid.
  is_regular_grid(xy_cols, digits = digits)

  x_arrange <- prepared$x_arrange

  # Resolve CRS before creating the SpatRaster.
  crs <- resolve_spatraster_crs(crs, attr(x, "crs"))

  # Non-numeric layers need the slower path below.
  col_classes <- unlist(lapply(values, is.numeric))

  # If all columns are numeric, create the raster directly.
  if (all(col_classes)) {
    newrast <- terra::rast(
      x_arrange,
      crs = crs,
      ...,
      type = "xyz",
      digits = digits
    )

    names(newrast) <- layer_names
    return(newrast)
  }

  # Create a template raster with an index for values.
  xyvalind <- x_arrange[, 1:2]
  xyvalind$valindex <- seq_len(nrow(xyvalind))

  values_w_ind <- x_arrange[, -c(1, 2)]
  values_w_ind$valindex <- xyvalind$valindex

  # Create the template.
  r_temp <- terra::rast(xyvalind, crs = crs, ..., type = "xyz", digits = digits)

  # Expand the grid and attach values.
  r_temp_df <- terra::as.data.frame(r_temp, na.rm = FALSE, xy = FALSE)
  r_temp_df <- tibble::as_tibble(r_temp_df)

  r_temp_df <- dplyr::left_join(r_temp_df, values_w_ind, by = "valindex")

  values <- r_temp_df[, -1]

  # Assign values to the raster.
  terra::nlyr(r_temp) <- 1

  build_raster_layers(r_temp, values, layer_names)
}

#' Rebuild objects created with `as_tbl_spat_attr()` to `SpatRaster`.
#'
#' This strict helper uses stored attributes to create a `SpatRaster` template
#' and then transfers the values.
#'
#' @noRd
as_spatrast_attr <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(x)
  }

  # Materialize lazy tables before reading reconstruction attributes.
  x <- data.table::as.data.table(x)

  # Retrieve the stored reconstruction attributes.
  attrs <- attributes(x)

  # Extract layer values from the non-coordinate columns.
  values <- dplyr::select(x, -c(1, 2))
  values <- data.table::as.data.table(values)

  # Create one raster per layer and assign values.
  build_raster_layers(
    function() {
      terra::rast(
        nrows = attrs$dims[1],
        ncols = attrs$dims[2],
        nlyrs = 1,
        crs = attrs$crs,
        extent = attrs$ext,
        resolution = attrs$res
      )
    },
    values,
    names(values)
  )
}

prepare_spatraster_cols <- function(x, xycols) {
  xy_cols <- x[xycols]
  values <- x[-xycols]

  names(xy_cols) <- c("x", "y")
  layer_names <- names(values)
  names(values) <- paste0("lyr", seq_len(ncol(values)))

  list(
    xy_cols = xy_cols,
    values = values,
    layer_names = layer_names,
    x_arrange = dplyr::bind_cols(xy_cols, values)
  )
}

resolve_spatraster_crs <- function(crs, crs_attr) {
  crs <- pull_crs(crs)

  if (is.na(crs)) {
    crs <- crs_attr
  }

  if (is.na(pull_crs(crs))) {
    crs <- NA
  }

  crs
}

build_raster_layers <- function(template, values, layer_names) {
  if (!is.function(template)) {
    template <- local({
      raster_template <- template
      function() raster_template
    })
  }

  temp_list <- lapply(seq_len(ncol(values)), function(i) {
    terra::setValues(template(), unlist(values[, i]))
  })

  defortify <- do.call("c", temp_list)
  names(defortify) <- layer_names
  defortify
}
