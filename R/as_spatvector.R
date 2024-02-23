#' Method for coercing objects to `SpatVector`
#'
#' @description
#'
#' `as_spatvector()` turns an existing  object into a `SpatVector`. This is a
#' wrapper of [terra::vect()] S4 method for signature `data.frame`.
#'
#' @return
#' A `SpatVector`.
#'
#' @export
#'
#' @name as_spatvector
#' @rdname as_spatvector
#'
#' @param x A [tibble][tibble::tibble()], data frame or \CRANpkg{sf} object of
#'  class [`sf`][sf::st_read()] or [`sfc`][sf::st_sfc()].
#'
#' @param ... additional arguments passed on to [terra::vect()].
#'
#' @param geom character. The field name(s) with the geometry data. Either
#'   two names for x and y coordinates of points, or a single name for a single
#'   column with WKT geometries.
#'
#' @param crs A crs on several formats (PROJ.4, WKT, EPSG code, ..) or
#'   and spatial object from sf or terra that includes the target coordinate
#'   reference system. See [pull_crs()] and **Details**.
#'
#'
#' @details
#'
#' This function differs from [terra::vect()] on the following:
#'
#' * geometries with `NA` or `""` values are removed prior to conversion
#' * If `x` is a grouped data frame (see [dplyr::group_by()]) the grouping
#'   vars are transferred and a "grouped" `SpatVector` is created (see
#'   [group_by.SpatVector()]).
#' * If no `crs` is provided and the tibble has been created with the method
#'   [as_tibble.SpatVector()], the `crs` is inferred from
#'   [`attr(x, "crs")`][attr()].
#' * Handles correctly the conversion of `EMPTY` geometries between
#'   \CRANpkg{sf} and \CRANpkg{terra}.
#'
#'
#' @family coerce
#'
#' @seealso
#'
#' [pull_crs()] for retrieving crs, and the corresponding utils [sf::st_crs()]
#' and [terra:;crs()].
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

  # Work always with tibble
  tbl <- as_tibble(x)

  # Issue: Convert coords to numeric if x,y
  # With tibble and integer terra::vect() gives errors
  if (length(geom) == 2) {
    tbl[[geom[1]]] <- as.double(tbl[[geom[1]]])
    tbl[[geom[2]]] <- as.double(tbl[[geom[2]]])
  }

  # Issue: Convert single geom to char and change blanks for NAs
  if (length(geom) == 1) {
    val <- as.character(tbl[[geom]])
    val[val == ""] <- NA
    tbl[[geom]] <- val
  }

  # Issue: Remove empty geoms
  tbl_end <- tidyr::drop_na(tbl, dplyr::all_of(geom))

  if (nrow(tbl) != nrow(tbl_end)) {
    cli::cli_alert_warning(paste(
      "Removed {nrow(tbl) - nrow(tbl_end)} row{?s} with empty",
      "{.arg geom}{qty({length(geom)})} column{?s} {.val {geom}}"
    ))
  }

  # Create SpatVector
  # crs
  crs_attr <- attr(x, "crs")
  crs <- pull_crs(crs)

  # Check from attrs
  if (is.na(crs)) crs <- crs_attr

  if (is.na(pull_crs(crs))) crs <- NA


  v <- terra::vect(tbl_end, geom = geom, crs = crs, ...)

  # Make groups
  if (dplyr::is_grouped_df(x) || is_rowwise_df(x)) {
    v <- group_prepare_spat(v, x)
  }



  return(v)
}

#' @rdname as_spatvector
#' @export
as_spatvector.sf <- function(x, ...) {
  # If none is empty then can convert safely

  if (!any(sf::st_is_empty(x))) {
    v <- terra::vect(x)
    v <- group_prepare_spat(v, x)
    return(v)
  }


  sf_col <- attr(x, "sf_column")

  # Create template with basic metadata
  template <- as_tbl_internal(terra::vect(x[!sf::st_is_empty(x), sf_col]))
  attr_template <- attributes(template)


  # Get tibble
  tbl <- sf::st_drop_geometry(x)

  # Check and manipulate empty geoms
  gg <- as.character(sf::st_as_text(sf::st_geometry(x)))

  if (any(sf::st_is_empty(x))) {
    gtype <- tolower(attr_template$geomtype)

    # Needs to be MULTI except for POINT
    empty_geom <- switch(gtype,
      "polygons" = "MULTIPOLYGON EMPTY",
      "lines" = "MULTILINESTRING EMPTY",
      "POINT EMPTY"
    )


    gg[sf::st_is_empty(x)] <- empty_geom
  }

  # Rebuild tibble
  dfgeom <- data.frame(x = gg)
  names(dfgeom) <- sf_col
  final_tibble <- dplyr::bind_cols(tbl, dfgeom)

  # Add grouping if already grouped
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
as_spatvector.sfc <- function(x, ...) {
  x_df <- sf::st_as_sf(x)

  as_spatvector(x_df)
}

#' @rdname as_spatvector
#' @export
as_spatvector.SpatVector <- function(x, ...) {
  return(x)
}


#' Rebuild objects created with as_tbl_internal to Spatvector
#' Strict version, used attributes for creating a template
#' SpatVector and then transfer the values
#' @noRd
as_spatvect_attr <- function(x) {
  if (inherits(x, "SpatVector")) {
    return(x)
  }

  # Get attributes
  attrs <- attributes(x)

  # Check if any geometry is NA or "" and replace by the corresponding value

  gg <- as.character(x[["geometry"]])

  gg[gg == ""] <- NA

  if (any(is.na(gg))) {
    gtype <- tolower(attrs$geomtype)

    # Needs to be MULTI except for POINT
    empty_geom <- switch(gtype,
      "polygons" = "MULTIPOLYGON EMPTY",
      "lines" = "MULTILINESTRING EMPTY",
      "POINT EMPTY"
    )

    gg[is.na(gg)] <- empty_geom

    x$geometry <- gg
  }

  v <- terra::vect(x, geom = "geometry", crs = attrs$crs)

  # Make groups
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
