#' Coerce a Spat* object to data frames
#'
#' @description
#'
#' `as_tibble()` method for SpatRaster and SpatVector.
#'
#' @rdname as_tibble
#' @name as_tibble
#'
#' @return
#' A tibble.
#'
#' @param x A SpatRaster created with [terra::rast()] or a SpatVector
#'   created with [terra::vect()].
#' @inheritParams terra::as.data.frame
#' @param ... Arguments passed on to [terra::as.data.frame()]
#' @param .name_repair Treatment of problematic column names:
#'   * `"minimal"`: No name repair or checks, beyond basic existence,
#'   * `"unique"`: Make sure names are unique and not empty,
#'   * `"check_unique"`: (default value), no name repair, but check they are
#'     `unique`,
#'   * `"universal"`: Make the names `unique` and syntactic
#'   * a function: apply custom name repair (e.g., `.name_repair = make.names`
#'     for names in the style of base R).
#'   * A purrr-style anonymous function, see [rlang::as_function()]
#'
#' @export
#' @importFrom terra as.data.frame
#' @importFrom tibble as_tibble
#'
#' @seealso [tibble::as_tibble()], [terra::as.data.frame()]
#'
#' @family coerce
#'
#' @section terra equivalent:
#'
#' [terra::as.data.frame()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tibble::as_tibble()] function.
#'
#' ## SpatRaster
#'
#' `r lifecycle::badge('questioning')` The tibble is returned with an attribute
#' including the crs of the initial object in WKT format (see [pull_crs()]).
#'
#' @section About layer names:
#'
#' When coercing SpatRaster objects to data frames, `x` and `y` names are
#' reserved for geographic coordinates of each cell of the raster. It should be
#' also noted that terra allows layers with duplicated names.
#'
#' In the process of coercing a SpatRaster to a tibble, tidyterra may rename
#' the layers of your SpatRaster for overcoming this issue. Specifically, layers
#' may be renamed on the following cases:
#'
#' - Layers with duplicated names
#' - When coercing to a tibble, if `xy = TRUE`, layers named `x` or `y` would be
#'   renamed.
#' - When working with tidyverse methods (i.e. [filter.SpatRaster()]), the
#'   latter would happen as well.
#'
#' tidyterra would display a message informing of the changes on the names of
#' the layer.
#'
#'
#' @examples
#'
#' library(terra)
#' # SpatRaster
#' f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#' r <- rast(f)
#'
#' as_tibble(r, na.rm = TRUE)
#'
#' as_tibble(r, xy = TRUE)
#'
#' # SpatVector
#'
#' f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#' v <- vect(f)
#'
#' as_tibble(v)
#'
as_tibble.SpatRaster <- function(x, ...,
                                 xy = FALSE,
                                 na.rm = FALSE,
                                 .name_repair = "unique") {
  if (xy) x <- make_layer_names(x)

  df <- tibble::as_tibble(terra::as.data.frame(x, ..., xy = xy, na.rm = na.rm),
    .name_repair = .name_repair
  )

  # Handle nas
  df[is.na(df)] <- NA


  # Set attributes
  attr(df, "crs") <- terra::crs(x)

  return(df)
}

#' @export
#' @rdname as_tibble
as_tibble.SpatVector <- function(x, ..., .name_repair = "unique") {
  df <- tibble::as_tibble(terra::as.data.frame(x, ...),
    .name_repair = .name_repair
  )

  return(df)
}


#' Strict internal version, returns a tibble with required attributes to
#' rebuild a SpatRaster
#' This is the underlying object that would be handled by the tidyverse
#' @noRd
as_tbl_spat_attr <- function(x) {
  if (isTRUE((attr(x, "source")) == "tbl_terra_spatraster")) {
    return(x)
  }

  if (!inherits(x, "SpatRaster")) cli::cli_abort("x is not a SpatRaster")
  
  x <- make_layer_names(x)

  todf <- data.table::as.data.table(x, xy = TRUE, na.rm = FALSE)

  # Set attributes
  attr(todf, "source") <- "tbl_terra_spatraster"

  attr(todf, "crs") <- terra::crs(x)
  # Extent
  attr(todf, "ext") <- c(
    terra::xmin(x),
    terra::xmax(x),
    terra::ymin(x),
    terra::ymax(x)
  )
  # Dimensions
  attr(todf, "dims") <- as.double(dim(x))
  attr(todf, "res") <- as.double(terra::res(x))

  return(todf)
}

# Make layer names protecting x and y, since are reserved
make_layer_names <- function(x) {
  # x and y names are reserved for coords
  init_names <- names(x)

  dup_names <- length(unique(init_names)) != terra::nlyr(x)


  if (!any(
    "x" %in% init_names,
    "y" %in% init_names,
    dup_names
  )) {
    return(x)
  }


  cli::cli_alert_info(paste(
    "Layer(s) with duplicated/reserved names detected.",
    "See `About layer names` section on",
    "`?as_tibble.SpatRaster`", "\n"
  ))
  cli::cli_alert_warning("Renaming layers:")

  names_with_coords <- c("x", "y", init_names)
  # Make new names
  newnames <- make.names(names_with_coords, unique = TRUE)

  newnames <- newnames[-c(1:2)]

  # Make new names
  message(crayon::black("New layer names:"))
  message(crayon::black(
    paste0("`", init_names, "` -> `", newnames, "`", collapse = "\n")
  ))
  message(crayon::black("\n"))

  names(x) <- newnames
  return(x)
}
