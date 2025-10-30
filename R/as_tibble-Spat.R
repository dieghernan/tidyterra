#' Coerce a `SpatVector` or `SpatRaster` object to data frames
#'
#' @description
#'
#' [as_tibble()] methods for `SpatRaster` and `SpatVector` objects.
#'
#' @rdname as_tibble.Spat
#' @name as_tibble.Spat
#'
#' @return
#' A [`tibble`][tibble::tibble()].
#'
#' @param x A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()].
#' @inheritParams terra::as.data.frame
#' @param ... Arguments passed on to [terra::as.data.frame()].
#' @param .name_repair Treatment of problematic column names:
#'   * `"minimal"`: No name repair or checks, beyond basic existence.
#'   * `"unique"`: Make sure names are unique and not empty.
#'   * `"check_unique"`: (default value), no name repair, but check they are
#'     `unique`.
#'   * `"universal"`: Make the names `unique` and syntactic.
#'   * a function: apply custom name repair (e.g., `.name_repair = make.names`
#'     for names in the style of base **R**).
#'   * A purrr-style anonymous function, see [rlang::as_function()].
#'
#' @export
#' @importFrom terra as.data.frame
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#'
#' @seealso [tibble::as_tibble()], [terra::as.data.frame()]
#'
#' @family coerce
#' @family tibble.methods
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::as.data.frame()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tibble::as_tibble()] function.
#'
#' ## `SpatRaster` and `SpatVector`
#'
#' The tibble is returned with an attribute including the crs of the initial
#' object in WKT format (see [pull_crs()]).
#'
#' @section About layer/column names:
#'
#' When coercing `SpatRaster` objects to data frames, `x` and `y` names are
#' reserved for geographic coordinates of each cell of the `SpatRaster` It
#' should be also noted that \CRANpkg{terra} allows layers with duplicated
#' names.
#'
#' In the process of coercing a `SpatRaster` to a tibble, \CRANpkg{tidyterra}
#' may rename the layers of your `SpatRaster` for overcoming this issue.
#' Specifically, layers may be renamed on the following cases:
#' * Layers with duplicated names.
#' * When coercing to a tibble, if `xy = TRUE`, layers named `x` or `y` would be
#'   renamed.
#' * When working with tidyverse methods (i.e. [filter.SpatRaster()]), the
#'   latter would happen as well.
#'
#' \CRANpkg{tidyterra} would display a message informing of the changes on the
#' names of the layer.
#'
#' The same issue happens for `SpatVector` with names `geometry` (when
#' `geom = c("WKT", "HEX")`) and `x`, `y` (when `geom = "XY"`). These are
#' reserved names representing the geometry of the `SpatVector` (see
#' [terra::as.data.frame()]). If `geom` is not `NULL` then the logic described
#' for `SpatRaster` would apply as well for the columns of the `SpatVector`.
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
as_tibble.SpatRaster <- function(
  x,
  ...,
  xy = FALSE,
  na.rm = FALSE,
  .name_repair = "unique"
) {
  if (xy) {
    x <- make_safe_names(x)
  }

  df <- tibble::as_tibble(
    terra::as.data.frame(x, ..., xy = xy, na.rm = na.rm),
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
    x <- make_safe_names(x, geom = geom)
  }

  df <- tibble::as_tibble(
    terra::as.data.frame(x, geom = geom, ...),
    .name_repair = .name_repair
  )

  # Grouped
  if (is_grouped_spatvector(x)) {
    # Add class
    class(df) <- c("grouped_df", class(df))
    attr(df, "groups") <- attr(x, "groups")

    # Validate
    dplyr::validate_grouped_df(df)
  }

  # Rowwise
  if (is_rowwise_spatvector(x)) {
    # Add class
    class(df) <- c("rowwise_df", class(df))
    attr(df, "groups") <- attr(x, "groups")
  }

  df <- check_regroups(df)

  # Set attributes if present
  if (!is.na(pull_crs(x))) {
    attr(df, "crs") <- pull_crs(x)
  }

  return(df)
}

as_tbl_internal <- function(x) {
  if (!inherits(x, c("SpatRaster", "SpatVector"))) {
    cli::cli_abort(
      "{.arg x} is not of {.cls SpatRaster} or {.cls SpatVector} object"
    )
  }

  if (inherits(x, "SpatRaster")) {
    as_tbl_spat_attr(x)
  } else {
    as_tbl_vector_internal(x)
  }
}

#' Strict internal version, returns a tibble with required attributes to
#' rebuild a `SpatRaster`
#' This is the underlying object that would be handled by the tidyverse
#' @noRd
as_tbl_spat_attr <- function(x) {
  x <- make_safe_names(x)

  todf <- data.table::as.data.table(x, xy = TRUE, na.rm = FALSE)
  todf[is.na(todf)] <- NA

  # Set attributes
  attr(todf, "source") <- "SpatRaster"

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

  todf
}


#' Strict internal version, returns a tibble with required attributes to
#' rebuild a `SpatVector`
#' This is the underlying object that would be handled by the tidyverse
#' @noRd
as_tbl_vector_internal <- function(x) {
  x <- make_safe_names(x, geom = "WKT")

  todf <- as.data.frame(x, geom = "WKT")
  todf[is.na(todf)] <- NA
  todf <- tibble::as_tibble(todf)

  # Grouped
  if (is_grouped_spatvector(x)) {
    # Add class
    class(todf) <- c("grouped_df", class(todf))
    attr(todf, "groups") <- attr(x, "groups")

    # Validate
    dplyr::validate_grouped_df(todf)
  }

  # Rowwise
  if (is_rowwise_spatvector(x)) {
    # Add class
    class(todf) <- c("rowwise_df", class(todf))
    attr(todf, "groups") <- attr(x, "groups")
  }

  todf <- check_regroups(todf)

  # Set attributes
  attr(todf, "source") <- "SpatVector"
  attr(todf, "crs") <- terra::crs(x)
  attr(todf, "geomtype") <- terra::geomtype(x)

  todf
}


# Protect reserved names on coercion
make_safe_names <- function(x, geom = NULL, messages = TRUE) {
  init_names <- names(x)

  if (inherits(x, "SpatRaster")) {
    dup_names <- length(unique(init_names)) != terra::nlyr(x)
    for_message <- "Layer(s)"
    geom <- "XY"
  } else if (inherits(x, "SpatVector")) {
    dup_names <- length(unique(init_names)) != terra::ncol(x)
    for_message <- "Column(s)"
  }

  if (geom == "XY") {
    if (!any("x" %in% init_names, "y" %in% init_names, dup_names)) {
      return(x)
    }
  } else if (!any("geometry" %in% init_names, dup_names)) {
    return(x)
  }

  if (messages) {
    cli::cli_alert_info(
      paste(
        for_message,
        "with duplicated/reserved names detected.",
        "See {.strong About layer/column names} section on",
        "{.fun tidyterra::as_tibble.SpatRaster}"
      ),
      wrap = TRUE
    )
    cli::cli_alert_warning("Renaming columns:")
  }
  if (geom == "XY") {
    names_with_coords <- c("x", "y", init_names)
    # Make new names
    newnames <- make.names(names_with_coords, unique = TRUE)

    newnames <- newnames[-c(1, 2)]
  } else {
    names_with_coords <- c("geometry", init_names)
    # Make new names
    newnames <- make.names(names_with_coords, unique = TRUE)

    newnames <- newnames[-1]
  }
  # Make new names
  if (messages) {
    names_changed <- !newnames == init_names

    msg <- paste0(
      "`",
      init_names[names_changed],
      "` -> `",
      newnames[names_changed],
      "`"
    )

    cli::cat_bullet(msg)
  }
  names(x) <- newnames
  return(x)
}

#' @export
tibble::as_tibble


#' Validate construction of groups. This is needed since that mixing terra
#' syntax with tidy syntax can modify group data (i.e. remove columns, change
#' number of rows) that won't be captured by tidyterra
#'
#' @noRd
check_regroups <- function(x) {
  if (dplyr::is_grouped_df(x)) {
    gvars <- dplyr::group_vars(x)
    val_vars <- gvars %in% names(x)
    all_vars <- all(val_vars)
    any_var <- any(val_vars)

    if (isFALSE(any_var)) {
      cli::cli_alert_warning(paste(
        "{.fun tidyterra::group_vars.SpatVector} missing on data.",
        " Have you mixed {.pkg terra} and {.pkg tidyterra} syntax?"
      ))
      cli::cli_bullets(c(i = "Ungrouping data"))
      return(dplyr::ungroup(x))
    }

    if (isFALSE(all_vars)) {
      regroup_vars <- gvars[val_vars]

      ung <- dplyr::ungroup(x)
      return(dplyr::group_by(ung, across_all_of(regroup_vars)))
    }

    # Check rows have been kept
    dif_rows <- all(sum(group_size(x)) == nrow(x))

    if (isFALSE(dif_rows)) {
      regroup_vars <- gvars[val_vars]
      ung <- dplyr::ungroup(x)
      return(dplyr::group_by(ung, across_all_of(regroup_vars)))
    }

    return(x)
  }

  if (is_rowwise_df(x)) {
    gvars <- dplyr::group_vars(x)

    # Does not need return vars in rowwise

    if (identical(gvars, character(0))) {
      # Check rows have been kept
      dif_rows <- all(sum(group_size(x)) == nrow(x))

      if (isFALSE(dif_rows)) {
        ung <- dplyr::ungroup(x)
        return(dplyr::rowwise(x))
      }

      return(x)
    }

    val_vars <- gvars %in% names(x)
    all_vars <- all(val_vars)
    any_var <- any(val_vars)

    if (isFALSE(any_var)) {
      cli::cli_alert_warning(paste(
        "{.fun tidyterra::group_vars.SpatVector} missing on data.",
        " Have you mixed {.pkg terra} and {.pkg tidyterra} syntax?"
      ))
      cli::cli_bullets(c(i = "Ungrouping data"))
      return(dplyr::ungroup(x))
    }

    if (isFALSE(all_vars)) {
      regroup_vars <- gvars[val_vars]

      ung <- dplyr::ungroup(x)
      return(dplyr::rowwise(ung, dplyr::all_of(regroup_vars)))
    }

    # Check rows have been kept
    dif_rows <- all(sum(group_size(x)) == nrow(x))

    if (isFALSE(dif_rows)) {
      regroup_vars <- gvars[val_vars]
      ung <- dplyr::ungroup(x)
      return(dplyr::rowwise(ung, dplyr::all_of(regroup_vars)))
    }
    return(x)
  }

  x
}
