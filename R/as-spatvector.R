#' Coerce objects to `SpatVector`
#'
#' @description
#'
#' `as_spatvector()` turns an existing object into a `SpatVector`. It wraps the
#' [terra::vect()] S4 method for the `data.frame` signature.
#'
#' @return
#' A `SpatVector`.
#'
#' @export
#' @encoding UTF-8
#'
#' @name as_spatvector
#' @rdname as_spatvector
#'
#' @param x A [tibble][tibble::tbl_df], data frame or \CRANpkg{sf} object of
#'   class [`sf`][sf::st_sf] or [`sfc`][sf::st_sfc].
#'
#' @param ... Additional arguments passed on to [terra::vect()].
#'
#' @param geom Character vector naming the fields that contain the geometry
#'   data. Use two names for point coordinates (`x` and `y`), or one name for a
#'   column with WKT geometries.
#'
#' @inheritParams as_spatraster
#'
#' @details
#'
#' This function differs from [terra::vect()] in the following ways:
#'
#' * Rows with geometry values `NA` or `""` are removed before conversion.
#' * If `x` is a grouped data frame (see [dplyr::group_by()]), the grouping
#'   variables are transferred and a grouped `SpatVector` is created (see
#'   [group_by.SpatVector()]).
#' * If no `crs` is provided and the tibble has been created with the method
#'   [as_tibble.SpatVector()], the `crs` is inferred from
#'   [`attr(x, "crs")`][attr()].
#' * It handles the conversion of `EMPTY` geometries between
#'   \CRANpkg{sf} and \CRANpkg{terra}.
#'
#' @family coerce
#'
#' @seealso
#'
#' [pull_crs()] for retrieving CRS and the corresponding utils [sf::st_crs()]
#' and [terra::crs()].
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::vect()]
#'
#' @examples
#' library(terra)
#'
#' v <- vect(matrix(1:80, ncol = 2), crs = "EPSG:3857")
#'
#' v$cat <- sample(LETTERS[1:4], size = nrow(v), replace = TRUE)
#'
#' v
#'
#' # Create tibble
#' as_tbl <- as_tibble(v, geom = "WKT")
#'
#' as_tbl
#'
#' # From tibble
#' newvect <- as_spatvector(as_tbl, geom = "geometry", crs = "EPSG:3857")
#' newvect
#'
as_spatvector <- function(x, ...) {
  UseMethod("as_spatvector")
}

#' @rdname as_spatvector
#' @export
#' @encoding UTF-8
as_spatvector.data.frame <- function(x, ..., geom = c("lon", "lat"), crs = "") {
  if (!length(geom) %in% c(1, 2)) {
    cli::cli_abort(paste(
      "{.arg geom} should be of length {.val {as.integer(1)}} or",
      "{.val {as.integer(2)}}, not {.val {length(geom)}}"
    ))
  }

  if (!is.character(geom)) {
    cli::cli_abort(paste(
      "{.arg geom} should be a {.cls character}, not",
      "{.cls {class(geom)}}"
    ))
  }

  if (!all(geom %in% names(x))) {
    cli::cli_abort(
      "Column{?s} {.var {setdiff(geom, names(x))}} not found in {.arg x}"
    )
  }

  # Always work with a tibble.
  tbl <- as_tibble(x)

  # Convert point coordinates to numeric. Integer columns can fail in
  # `terra::vect()`.
  if (length(geom) == 2) {
    tbl[[geom[1]]] <- as.double(tbl[[geom[1]]])
    tbl[[geom[2]]] <- as.double(tbl[[geom[2]]])
  }

  # Convert a single geometry column to character and treat blanks as `NA`.
  if (length(geom) == 1) {
    val <- as.character(tbl[[geom]])
    val[val == ""] <- NA
    tbl[[geom]] <- val
  }

  # Remove rows with missing geometry values.
  tbl_end <- tidyr::drop_na(tbl, dplyr::all_of(geom))

  if (nrow(tbl) != nrow(tbl_end)) {
    cli::cli_alert_warning(paste(
      "Removed {nrow(tbl) - nrow(tbl_end)} row{?s} with empty",
      "{.arg geom}{qty({length(geom)})} column{?s} {.val {geom}}"
    ))
  }

  # Resolve the CRS before creating the `SpatVector`.
  crs_attr <- attr(x, "crs")
  crs <- pull_crs(crs)

  # Fall back to the CRS stored in the input attributes.
  if (is.na(crs)) {
    crs <- crs_attr
  }

  if (anyNA(pull_crs(crs))) {
    crs <- ""
  }

  v <- terra::vect(tbl_end, geom = geom, crs = crs, ...)

  # Remove the CRS if none was supplied.
  if (crs == "") {
    terra::crs(v) <- NULL
  }

  # Restore grouping metadata when present.
  if (dplyr::is_grouped_df(x) || is_rowwise_df(x)) {
    v <- group_prepare_spat(v, x)
  }

  v
}

#' @rdname as_spatvector
#' @export
#' @encoding UTF-8
as_spatvector.sf <- function(x, ...) {
  # Convert directly when there are no empty geometries.

  if (!any(sf::st_is_empty(x))) {
    v <- terra::vect(x)
    v <- group_prepare_spat(v, x)
    return(v)
  }

  sf_col <- attr(x, "sf_column")

  # Create a template with the required metadata.
  template <- as_tbl_internal(terra::vect(x[!sf::st_is_empty(x), sf_col]))
  attr_template <- attributes(template)

  # Extract the non-geometry columns.
  tbl <- sf::st_drop_geometry(x)

  # Replace empty geometries with valid empty WKT values.
  gg <- as.character(sf::st_as_text(sf::st_geometry(x)))

  if (any(sf::st_is_empty(x))) {
    gtype <- tolower(attr_template$geomtype)

    # Use MULTI geometries except for points.
    empty_geom <- switch(gtype,
      "polygons" = "MULTIPOLYGON EMPTY",
      "lines" = "MULTILINESTRING EMPTY",
      "POINT EMPTY"
    )

    gg[sf::st_is_empty(x)] <- empty_geom
  }

  # Rebuild the tibble with the geometry column.
  dfgeom <- data.frame(x = gg)
  names(dfgeom) <- sf_col
  final_tibble <- dplyr::bind_cols(tbl, dfgeom)

  # Restore grouping when the input is grouped.
  if (dplyr::is_grouped_df(x)) {
    vars_sf <- dplyr::group_vars(x)
    final_tibble <- dplyr::group_by(final_tibble, across_all_of(vars_sf))
  }

  if (is_rowwise_df(x)) {
    vars <- group_vars(x)
    final_tibble <- dplyr::rowwise(final_tibble, dplyr::all_of(vars))
  }

  rm(gg)

  as_spatvector(final_tibble, geom = sf_col, crs = pull_crs(attr_template$crs))
}

#' @rdname as_spatvector
#' @export
#' @encoding UTF-8
as_spatvector.sfc <- function(x, ...) {
  x_df <- sf::st_as_sf(x)

  as_spatvector(x_df)
}

#' @rdname as_spatvector
#' @export
#' @encoding UTF-8
as_spatvector.SpatVector <- function(x, ...) {
  x
}

#' Rebuild objects created with `as_tbl_internal()` to `SpatVector`
#' This strict internal helper uses the stored attributes to recreate a
#' template `SpatVector` and then transfer the values.
#' @noRd
as_spatvect_attr <- function(x) {
  if (inherits(x, "SpatVector")) {
    return(x)
  }

  # Retrieve the stored attributes.
  attrs <- attributes(x)

  # Replace missing or blank geometries with a matching empty geometry.

  gg <- as.character(x[["geometry"]])

  gg[gg == ""] <- NA

  if (anyNA(gg)) {
    gtype <- tolower(attrs$geomtype)

    # Use MULTI geometries except for points.
    empty_geom <- switch(gtype,
      "polygons" = "MULTIPOLYGON EMPTY",
      "lines" = "MULTILINESTRING EMPTY",
      "POINT EMPTY"
    )

    gg[is.na(gg)] <- empty_geom

    x$geometry <- gg
  }

  v <- terra::vect(x, geom = "geometry", crs = attrs$crs)

  # Restore grouping metadata when present.
  if (dplyr::is_grouped_df(x)) {
    vars <- dplyr::group_vars(x)

    v <- group_by(v, across_all_of(vars))
  }

  if (is_rowwise_df(x)) {
    vars <- group_vars(x)
    v <- rowwise(v, dplyr::all_of(vars))
  }

  v
}
