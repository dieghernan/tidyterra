#' Coerce `SpatRaster` and `SpatVector` objects to tibbles
#'
#' @description
#'
#' [as_tibble()] methods for `SpatRaster` and `SpatVector` objects.
#'
#' @rdname as_tibble.Spat
#' @name as_tibble.Spat
#'
#' @return
#' A [tibble][tibble::tbl_df].
#'
#' @param x A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()].
#' @param ... Arguments passed on to [terra::as.data.frame()].
#'
#' @inheritParams terra::as.data.frame
#' @inheritParams tibble::as_tibble
#'
#' @export
#' @encoding UTF-8
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
#' Implementation of the **generic** [tibble::as_tibble()] method.
#'
#' ## `SpatRaster` and `SpatVector`
#'
#' The returned tibble includes the CRS of the original object as an attribute
#' in WKT format (see [pull_crs()]).
#'
#' @section About layer/column names:
#'
#' When coercing `SpatRaster` objects to data frames, `x` and `y` are reserved
#' names for the geographic coordinates of each cell. \CRANpkg{terra} also
#' allows layers with duplicated names.
#'
#' When coercing a `SpatRaster` to a tibble, \CRANpkg{tidyterra} may rename its
#' layers to avoid these issues. Specifically, layers may be renamed in the
#' following cases:
#' * Layers with duplicated names.
#' * When coercing to a tibble, if `xy = TRUE`, layers named `x` or
#'   `y` are renamed.
#' * When working with tidyverse methods (i.e. [filter.SpatRaster()]), the
#'   same renaming would happen.
#'
#' \CRANpkg{tidyterra} displays a message describing the renamed layers.
#'
#' The same issue affects `SpatVector` objects with reserved names such as
#' `geometry` (when `geom = c("WKT", "HEX")`) and `x`, `y` (when
#' `geom = "XY"`). These names represent geometry columns in
#' [terra::as.data.frame()]. If `geom` is not `NULL`, the same renaming logic
#' described for `SpatRaster` also applies to `SpatVector` columns.
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
  .name_repair = c(
    "unique",
    "check_unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  )
) {
  if (xy) {
    x <- make_safe_names(x)
  }

  df <- tibble::as_tibble(
    terra::as.data.frame(x, ..., xy = xy, na.rm = na.rm),
    .name_repair = .name_repair
  )

  # Preserve missing values explicitly.
  df[is.na(df)] <- NA

  # Store the CRS as an attribute.
  attr(df, "crs") <- terra::crs(x)

  df
}

#' @export
#' @rdname as_tibble.Spat
as_tibble.SpatVector <- function(
  x,
  ...,
  geom = NULL,
  .name_repair = c(
    "unique",
    "check_unique",
    "universal",
    "minimal",
    "unique_quiet",
    "universal_quiet"
  )
) {
  if (!is.null(geom)) {
    x <- make_safe_names(x, geom = geom)
  }

  df <- tibble::as_tibble(
    terra::as.data.frame(x, geom = geom, ...),
    .name_repair = .name_repair
  )

  # Restore grouped data frame metadata.
  if (is_grouped_spatvector(x)) {
    # Add the grouped class.
    class(df) <- c("grouped_df", class(df))
    attr(df, "groups") <- attr(x, "groups")

    # Validate the grouped tibble.
    dplyr::validate_grouped_df(df)
  }

  # Restore rowwise metadata.
  if (is_rowwise_spatvector(x)) {
    # Add the rowwise class.
    class(df) <- c("rowwise_df", class(df))
    attr(df, "groups") <- attr(x, "groups")
  }

  df <- check_regroups(df)

  # Store the CRS when it is available.
  if (!is.na(pull_crs(x))) {
    attr(df, "crs") <- pull_crs(x)
  }

  df
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

#' Strict internal version that returns a tibble with the attributes required
#' to rebuild a `SpatRaster`.
#' This is the underlying object handled by tidyverse methods.
#' @noRd
as_tbl_spat_attr <- function(x) {
  x <- make_safe_names(x)

  todf <- data.table::as.data.table(x, xy = TRUE, na.rm = FALSE)
  todf[is.na(todf)] <- NA

  # Store the attributes required to rebuild the object.
  attr(todf, "source") <- "SpatRaster"

  attr(todf, "crs") <- terra::crs(x)
  # Store the extent.
  attr(todf, "ext") <- c(
    terra::xmin(x),
    terra::xmax(x),
    terra::ymin(x),
    terra::ymax(x)
  )
  # Store dimensions and resolution.
  attr(todf, "dims") <- as.double(dim(x))
  attr(todf, "res") <- as.double(terra::res(x))

  todf
}

#' Strict internal version that returns a tibble with the attributes required
#' to rebuild a `SpatVector`.
#' This is the underlying object handled by tidyverse methods.
#' @noRd
as_tbl_vector_internal <- function(x) {
  x <- make_safe_names(x, geom = "WKT")

  todf <- as.data.frame(x, geom = "WKT")
  todf[is.na(todf)] <- NA
  todf <- tibble::as_tibble(todf)

  # Restore grouped data frame metadata.
  if (is_grouped_spatvector(x)) {
    # Add the grouped class.
    class(todf) <- c("grouped_df", class(todf))
    attr(todf, "groups") <- attr(x, "groups")

    # Validate the grouped tibble.
    dplyr::validate_grouped_df(todf)
  }

  # Restore rowwise metadata.
  if (is_rowwise_spatvector(x)) {
    # Add the rowwise class.
    class(todf) <- c("rowwise_df", class(todf))
    attr(todf, "groups") <- attr(x, "groups")
  }

  todf <- check_regroups(todf)

  # Store the attributes required to rebuild the object.
  attr(todf, "source") <- "SpatVector"
  attr(todf, "crs") <- terra::crs(x)
  attr(todf, "geomtype") <- terra::geomtype(x)

  todf
}

# Protect reserved names during coercion.
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
    # Generate repaired names.
    newnames <- make.names(names_with_coords, unique = TRUE)

    newnames <- newnames[-c(1, 2)]
  } else {
    names_with_coords <- c("geometry", init_names)
    # Generate repaired names.
    newnames <- make.names(names_with_coords, unique = TRUE)

    newnames <- newnames[-1]
  }
  # Report renamed columns or layers.
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
  x
}

#' @export
tibble::as_tibble


#' Validate grouped output after reconstruction.
#' Mixing \CRANpkg{terra} syntax with tidy syntax can modify grouped data, for
#' example by removing columns or changing the number of rows, in ways that
#' \CRANpkg{tidyterra} cannot track automatically.
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

    # Rowwise data does not need grouping variables here.

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
