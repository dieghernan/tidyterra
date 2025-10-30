#' Glance at an `Spat*` object
#'
#' Glance accepts a model object and returns a [tibble::tibble()] with exactly
#' one row of `Spat`. The summaries are typically geographic information.
#'
#' @param x A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()].
#' @param ... Ignored by this method.
#'
#' @importFrom generics glance
#' @export
#'
#' @family generics.methods
#'
#' @return
#' glance methods always return a one-row data frame. See **Methods**.
#'
#' @rdname glance.Spat
#' @name glance.Spat
#'
#' @seealso
#' [`glimpse.Spat`], [generics::glance()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [generics::glance()] method for
#' `Spat*`. objects.
#'
#' @examples
#'
#' library(terra)
#'
#' # SpatVector
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' glance(v)
#'
#' # SpatRaster
#' r <- rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))
#'
#' glance(r)
#'
glance.SpatRaster <- function(x, ...) {
  initial <- tibble::tibble(
    nrow = terra::nrow(x),
    ncol = terra::ncol(x),
    nlyr = terra::nlyr(x),
    ncell = terra::ncell(x),
    xres = terra::xres(x),
    yres = terra::yres(x)
  )

  ex <- terra::ext(x)
  ex <- as.vector(ex)
  ex_df <- tibble::as_tibble_row(ex)
  initial <- dplyr::bind_cols(initial, ex_df)

  # CRS
  crsnamed <- get_named_crs(x)
  if (is.na(crsnamed)) {
    crsnamed <- "CRS: Not Defined / Empty"
  }

  initial$crs <- crsnamed
  initial$crs_units <- get_crs_units(x)

  # Get source
  f <- unique(terra::sources(x))
  f[f == ""] <- NA

  initial$source <- paste0(basename(f), collapse = ", ")

  # Additional
  initial$has_rgb <- terra::has.RGB(x)
  initial$has_colors <- any(terra::has.colors(x))
  initial$has_time <- any(terra::has.time(x))

  initial
}

#' @rdname glance.Spat
#' @export
glance.SpatVector <- function(x, ...) {
  initial <- tibble::tibble(
    geometry = terra::geomtype(x),
    nrow = terra::nrow(x),
    ncol = terra::ncol(x)
  )

  ex <- terra::ext(x)
  ex <- as.vector(ex)
  ex_df <- tibble::as_tibble_row(ex)
  initial <- dplyr::bind_cols(initial, ex_df)

  # Get source
  f <- unique(terra::sources(x))
  f[f == ""] <- NA

  initial$source <- paste0(basename(f), collapse = ", ")
  # CRS
  crsnamed <- get_named_crs(x)
  if (is.na(crsnamed)) {
    crsnamed <- "CRS: Not Defined / Empty"
  }

  initial$crs <- crsnamed
  initial$crs_units <- get_crs_units(x)
  initial
}

#' @export
generics::glance


# Helpers ----
get_crs_units <- function(x) {
  pulled_crs <- pull_crs(x)
  if (is.na(pulled_crs)) {
    return(NA)
  }

  if (sf::st_is_longlat(pulled_crs)) {
    return("degrees")
  }

  unts <- try(sf::st_crs(pulled_crs)$units, silent = TRUE)

  if (inherits(unts, "character")) {
    unitsdb <- unitsdb
    longname <- as.vector(unitsdb[unitsdb$abb == unts, ]$name)

    longname
  } else {
    NULL
  }
}
