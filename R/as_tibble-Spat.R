#' Coerce a Spat* object to data frames
#'
#' @description
#'
#' `as_tibble()` method for SpatRaster and SpatVector.
#'
#' @rdname as_tibble.Spat
#' @name as_tibble.Spat
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
#' @family tibble.methods
#'
#' @section terra equivalent:
#'
#' [terra::as.data.frame()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tibble::as_tibble()] function.
#'
#' ## SpatRaster and SpatVector
#'
#' `r lifecycle::badge('questioning')` The tibble is returned with an attribute
#' including the crs of the initial object in WKT format (see [pull_crs()]).
#'
#' @section About layer/column names:
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
#' The same issue happens for SpatVector with names `geometry` (when
#' `geom = c("WKT", "HEX")`) and `x`, `y` (when `geom = "XY"`). These are
#' reserved names representing the geometry of the SpatVector (see
#' [terra::as.data.frame()]). If `geom` is not `NULL` then the logic described
#' for SpatRaster would apply as well for the columns of the SpatVector.
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
#' @rdname as_tibble.Spat
as_tibble.SpatVector <- function(x, ..., geom = NULL, .name_repair = "unique") {
  if (!is.null(geom)) {
    x <- make_col_names(x, geom = geom)
  }


  df <- tibble::as_tibble(terra::as.data.frame(x, geom = geom, ...),
    .name_repair = .name_repair
  )

  # Grouped
  if (is_grouped_spatvector(x)) {
    vars <- group_vars(x)

    remove_gr <- df[, !grepl("dplyr.group", names(df))]

    df <- dplyr::group_by(remove_gr, dplyr::across(dplyr::all_of(vars)))
  }

  # Set attributes
  attr(df, "crs") <- terra::crs(x)

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
  todf[is.na(todf)] <- NA

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
    "See `About layer/column names` section on",
    "`?as_tibble.SpatRaster`", "\n"
  ))
  cli::cli_alert_warning("Renaming layers:")

  names_with_coords <- c("x", "y", init_names)
  # Make new names
  newnames <- make.names(names_with_coords, unique = TRUE)

  newnames <- newnames[-c(1:2)]

  # Make new names
  message(cli::col_black("New layer names:"))
  message(cli::col_black(
    paste0("`", init_names, "` -> `", newnames, "`", collapse = "\n")
  ))
  message(cli::col_black("\n"))

  names(x) <- newnames
  return(x)
}



#' Strict internal version, returns a tibble with required attributes to
#' rebuild a SpatVector
#' This is the underlying object that would be handled by the tidyverse
#' @noRd
as_tbl_spatvect_attr <- function(x) {
  if (isTRUE((attr(x, "source")) == "tbl_terra_spatvector")) {
    return(x)
  }

  if (!inherits(x, "SpatVector")) cli::cli_abort("x is not a SpatVector")

  x <- make_col_names(x, geom = "WKT")

  todf <- as.data.frame(x, geom = "WKT")
  todf[is.na(todf)] <- NA

  # Grouped
  if (is_grouped_spatvector(x)) {
    vars <- group_vars(x)

    remove_gr <- todf[, !grepl("dplyr.group", names(todf))]

    todf <- dplyr::group_by(remove_gr, dplyr::across(dplyr::all_of(vars)))
  } else {
    todf <- dplyr::as_tibble(todf)
  }

  # Set attributes
  attr(todf, "source") <- "tbl_terra_spatvector"
  attr(todf, "crs") <- terra::crs(x)
  attr(todf, "geomtype") <- terra::geomtype(x)

  return(todf)
}


# Make col names protecting geometry, since is reserved for SpatVectors
make_col_names <- function(x, geom, messages = TRUE) {
  # geometry name is reserved for geometry
  init_names <- names(x)

  dup_names <- length(unique(init_names)) != terra::ncol(x)

  if (geom == "XY") {
    if (!any("x" %in% init_names, "y" %in% init_names, dup_names)) {
      return(x)
    }
  } else if (!any("geometry" %in% init_names, dup_names)) {
    return(x)
  }

  if (messages) {
    cli::cli_alert_info(paste(
      "Layer(s) with duplicated/reserved names detected.",
      "See `About layer/column names` section on",
      "`?as_tibble.SpatRaster`", "\n"
    ))
    cli::cli_alert_warning("Renaming columns:")
  }
  if (geom == "XY") {
    names_with_coords <- c("x", "y", init_names)
    # Make new names
    newnames <- make.names(names_with_coords, unique = TRUE)

    newnames <- newnames[-c(1:2)]
  } else {
    names_with_coords <- c("geometry", init_names)
    # Make new names
    newnames <- make.names(names_with_coords, unique = TRUE)

    newnames <- newnames[-c(1)]
  }
  # Make new names
  if (messages) {
    message(cli::col_black("New column names:"))
    message(cli::col_black(
      paste0("`", init_names, "` -> `", newnames, "`", collapse = "\n")
    ))
    message(cli::col_black("\n"))
  }
  names(x) <- newnames
  return(x)
}

#' @export
tibble::as_tibble
