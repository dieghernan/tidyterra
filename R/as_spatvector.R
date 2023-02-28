#' Method for coercing objects to to SpatVector
#'
#' @description
#'
#' `as_spatvector()` turns an existing  object into a SpatVector. This is a
#' wrapper of [terra::vect()] S4 method.
#'
#' @return
#' A SpatVector.
#'
#' @export
#'
#' @name as_spatvector
#' @rdname as_spatvector
#'
#' @param x A tibble, data frame, sf or sfc object.
#'
#' @param ... additional arguments passed on to [terra::vect()].
#'
#' @param geom character. The field name(s) with the geometry data. Either
#'   two names for x and y coordinates of points, or a single name for a single
#'   column with WKT geometries.
#'
#' @param crs A crs on several formats (PROJ.4, WKT, EPSG code, ..) or
#'   and spatial object from sf or terra that includes the target coordinate
#'   reference system. See [pull_crs()]. See **Details**.
#'
#'
#' @details
#'
#' This function differs from [terra::vect()] on the following:
#'
#' - geometries with `NA/""` values are removed prior to conversion
#' - If `x` is a grouped data frame (see [dplyr::group_by()]) the grouping
#'   vars are transferred and a "grouped" SpatVector is created (see
#'   [group_by.SpatVector()]).
#' - If no `crs` is provided and the tibble has been created with the method
#'   [as_tibble.SpatVector()], the `crs` is inferred from `attr(x, "crs")`.
#' - Handles correctly the conversion of `EMPTY` geometries between \pkg{sf}
#'   and \pkg{terra}.
#'
#'
#' @family coerce
#'
#' @seealso [pull_crs()]
#'
#' @section terra equivalent:
#'
#' [terra::vect()]
#'
#' @examples
#' library(terra)
#'
#' v <- vect(matrix(1:80, ncol = 2), crs = "epsg:3857")
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
#' newvect <- as_spatvector(as_tbl, geom = "geometry", crs = "epsg:3857")
#' newvect
#'
as_spatvector <- function(x, ...) {
  UseMethod("as_spatvector")
}

#' @rdname as_spatvector
#' @export
as_spatvector.data.frame <- function(x, ..., geom = c("lon", "lat"), crs = "") {
  if (!length(geom) %in% c(1, 2)) {
    stop("geom should be of lenght 1 or 2")
  }

  if (!is.character(geom)) {
    stop("geom should be of type <character>")
  }

  if (!all(geom %in% names(x))) {
    stop(
      paste0("Column(s) <", paste0(setdiff(geom, names(x)), collapse = ",")),
      "> not found in x"
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
    message(paste0(
      "Removed ", nrow(tbl) - nrow(tbl_end),
      " row(s) with empty geom column(s) <",
      paste0(geom, collapse = ","), ">"
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
  if (dplyr::is_grouped_df(x)) {
    vars <- dplyr::group_vars(x)

    # Add groups metadata
    attr(v, "group_vars") <- vars
  }

  return(v)
}

#' @rdname as_spatvector
#' @export
as_spatvector.sf <- function(x, ...) {
  sf_col <- attr(x, "sf_column")

  # Create template with basic metadata
  template <- as_tbl_spatvect_attr(terra::vect(x[!sf::st_is_empty(x), sf_col]))
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
    final_tibble <- dplyr::group_by(
      final_tibble,
      dplyr::across(dplyr::all_of(vars_sf))
    )
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


#' Rebuild objects created with as_tbl_spatvect_attr to Spatvector
#' Strict version, used attributes for creating a template
#' SpatVector and then transfer the values
#' @noRd
as_spatvect_attr <- function(x) {
  if (inherits(x, "SpatVector")) {
    return(x)
  }


  if (!isTRUE((attr(x, "source")) == "tbl_terra_spatvector")) {
    cli::cli_alert_danger(
      paste(
        "Spatial attributes may have been removed.",
        "Check the result carefully."
      )
    )
    return(as_spatvector(x, geom = "geometry"))
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

    v <- group_by(v, dplyr::across(dplyr::all_of(vars)))
  }

  v
}
