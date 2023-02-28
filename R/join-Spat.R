#' Mutating joins for SpatVectors
#'
#' @description
#' Mutating joins add columns from `y` to `x`, matching observations based on
#' the keys. There are four mutating joins: the inner join, and the three outer
#' joins.
#'
#' See [dplyr::inner_join()] for details.
#'
#' @export
#' @rdname mutate-joins.SpatVector
#' @name mutate-joins.SpatVector
#'
#' @seealso [dplyr::inner_join()], [dplyr::left_join()], [dplyr::right_join()],
#' [dplyr::full_join()], [terra::merge()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr inner_join
#'
#' @param x A SpatVector created with [terra::vect()].
#' @param y A data frame or other object coercible to a data frame. **If a
#'   SpatVector of sf object** is provided it would return an error (see
#'   [terra::intersect()] for performing spatial joins).
#'
#' @inheritParams dplyr::full_join
#'
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' [terra::merge()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::inner_join()] family
#'
#' ## SpatVector
#'
#' The geometry column has a sticky behavior. This means that the result would
#' have always the geometry of `x` for the records that matches the join
#' conditions.
#'
#' Note that for [right_join()] and [full_join()] it is possible to return
#' empty geometries (since `y` is expected to be a data frame with no
#' geometries). Although this kind of joining operations may not be common on
#' spatial manipulation, it is possible that the function crashes, since
#' handling of `EMPTY` geometries differs on \pkg{terra} and \pkg{sf} (the
#' backend of `*_join.SpatVector()` is the implementation made on \pkg{sf}).
#'
#' @examples
#' library(terra)
#' library(ggplot2)
#' # Vector
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' # A data frame
#' df <- data.frame(
#'   cpro = sprintf("%02d", 1:10),
#'   x = 1:10,
#'   y = 11:20,
#'   letter = rep_len(LETTERS[1:3], length.out = 10)
#' )
#'
#' # Inner join
#' inner <- v %>% inner_join(df)
#'
#' nrow(inner)
#' autoplot(inner, aes(fill = letter)) + ggtitle("Inner Join")
#'
#'
#' # Left join
#'
#' left <- v %>% left_join(df)
#' nrow(left)
#'
#' autoplot(left, aes(fill = letter)) + ggtitle("Left Join")
#'
#'
#' # Right join
#' right <- v %>% right_join(df)
#' nrow(right)
#'
#' autoplot(right, aes(fill = letter)) + ggtitle("Right Join")
#'
#' # There are empty geometries, check with data from df
#' ggplot(right, aes(x, y)) +
#'   geom_point(aes(color = letter))
#'
#'
#' # Full join
#' full <- v %>% full_join(df)
#' nrow(full)
#'
#' autoplot(full, aes(fill = letter)) + ggtitle("Full Join")
#'
#' # Check with data from df
#' ggplot(full, aes(x, y)) +
#'   geom_point(aes(color = letter))
#'
inner_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ..., keep = NULL) {
  # Use sf method
  sf_obj <- sf::st_as_sf(x)

  if (inherits(y, "SpatVector")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`SpatVector`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, y)`")
    ))
  }

  if (inherits(y, "sf")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`sf`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, terra::vect(y))`")
    ))
  }
  y <- as.data.frame(y)

  joined <- dplyr::inner_join(sf_obj,
    y = y, by = by, copy = copy, suffix = suffix, ...,
    keep = keep
  )

  return(terra::vect(joined))
}

#' @export
dplyr::inner_join

#' @importFrom dplyr left_join
#' @export
#' @name mutate-joins.SpatVector
left_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ..., keep = NULL) {
  # Use sf method
  sf_obj <- sf::st_as_sf(x)

  if (inherits(y, "SpatVector")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`SpatVector`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, y)`")
    ))
  }

  if (inherits(y, "sf")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`sf`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, terra::vect(y))`")
    ))
  }

  y <- as.data.frame(y)
  joined <- dplyr::left_join(sf_obj,
    y = y, by = by, copy = copy, suffix = suffix, ...,
    keep = keep
  )

  return(terra::vect(joined))
}

#' @export
dplyr::left_join

#' @importFrom dplyr right_join
#' @export
#' @name mutate-joins.SpatVector
right_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ..., keep = NULL) {
  # Use sf method
  sf_obj <- sf::st_as_sf(x)

  if (inherits(y, "SpatVector")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`SpatVector`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, y)`")
    ))
  }

  if (inherits(y, "sf")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`sf`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, terra::vect(y))`")
    ))
  }

  y <- as.data.frame(y)
  joined <- dplyr::right_join(sf_obj,
    y = y, by = by, copy = copy, suffix = suffix, ...,
    keep = keep
  )


  # Need to remove empty geometries until terra and sf are compatibles
  # when handling empty geoms
  if (!all(sf::st_is_empty(joined))) {
    # Work with WKT
    geoms <- sf::st_geometry(joined)
    geoms_wkt <- sf::st_as_text(geoms)

    # Locate empty geoms
    empties <- sf::st_is_empty(geoms)

    # Assess type to use on empties
    geoms_types <- unique(sf::st_geometry_type(geoms[!empties]))

    is_point <- any(grepl("POINT", geoms_types))
    is_line <- any(grepl("LINESTRING", geoms_types))

    # Need MULTI for ensure conversions
    new_empty <- ifelse(is_point, "MULTIPOINT EMPTY",
      ifelse(is_line, "MULTILINESTRING EMPTY",
        "MULTIPOLYGON EMPTY"
      )
    )


    geoms_wkt_fixed <- geoms_wkt
    geoms_wkt_fixed[empties] <- new_empty

    # Coerce with wkb
    df <- sf::st_drop_geometry(joined)
    df$tterra_wkt <- geoms_wkt_fixed

    # Use WKT mode on terra
    joined <- terra::vect(df, geom = "tterra_wkt", crs = pull_crs(joined))

    return(joined)
  }


  return(terra::vect(joined))
}

#' @export
dplyr::right_join


#' @importFrom dplyr full_join
#' @export
#' @name mutate-joins.SpatVector
full_join.SpatVector <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ..., keep = NULL) {
  # Use sf method
  sf_obj <- sf::st_as_sf(x)

  if (inherits(y, "SpatVector")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`SpatVector`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, y)`")
    ))
  }

  if (inherits(y, "sf")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`sf`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, terra::vect(y))`")
    ))
  }

  y <- as.data.frame(y)
  joined <- dplyr::full_join(sf_obj,
    y = y, by = by, copy = copy, suffix = suffix, ...,
    keep = keep
  )




  # Need to remove empty geometries until terra and sf are compatibles
  # when handling empty geoms
  if (!all(sf::st_is_empty(joined))) {
    # Work with WKT
    geoms <- sf::st_geometry(joined)
    geoms_wkt <- sf::st_as_text(geoms)

    # Locate empty geoms
    empties <- sf::st_is_empty(geoms)

    # Assess type to use on empties
    geoms_types <- unique(sf::st_geometry_type(geoms[!empties]))

    is_point <- any(grepl("POINT", geoms_types))
    is_line <- any(grepl("LINESTRING", geoms_types))

    # Need MULTI for ensure conversions
    new_empty <- ifelse(is_point, "MULTIPOINT EMPTY",
      ifelse(is_line, "MULTILINESTRING EMPTY",
        "MULTIPOLYGON EMPTY"
      )
    )


    geoms_wkt_fixed <- geoms_wkt
    geoms_wkt_fixed[empties] <- new_empty

    # Coerce with wkb
    df <- sf::st_drop_geometry(joined)
    df$tterra_wkt <- geoms_wkt_fixed

    # Use WKT mode on terra
    joined <- terra::vect(df, geom = "tterra_wkt", crs = pull_crs(joined))

    return(joined)
  }


  return(terra::vect(joined))
}

#' @export
dplyr::full_join


#' Filtering joins for SpatVectors
#'
#' @description
#' Filtering joins filter rows from `x` based on the presence or absence of
#' matches in `y`:
#'  - [semi_join()] return all rows from `x` with a match in `y`.
#'  - [anti_join()] return all rows from `x` without a match in `y`.
#'
#' See [dplyr::semi_join()] for details.
#'
#' @export
#' @rdname filter-joins.SpatVector
#' @name filter-joins.SpatVector
#'
#' @seealso [dplyr::semi_join()], [dplyr::anti_join()], [terra::merge()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @importFrom dplyr semi_join
#'
#' @inheritParams mutate-joins.SpatVector
#'
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' [terra::merge()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::semi_join()] family
#'
#' ## SpatVector
#'
#' The geometry column has a sticky behavior. This means that the result would
#' have always the geometry of `x` for the records that matches the join
#' conditions.
#'
#' @examples
#' library(terra)
#' library(ggplot2)
#'
#' # Vector
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' # A data frame
#' df <- data.frame(
#'   cpro = sprintf("%02d", 1:10),
#'   x = 1:10,
#'   y = 11:20,
#'   letter = rep_len(LETTERS[1:3], length.out = 10)
#' )
#'
#' v
#'
#' # Semi join
#' semi <- v %>% semi_join(df)
#'
#' semi
#'
#' autoplot(semi, aes(fill = iso2)) + ggtitle("Semi Join")
#'
#'
#' # Anti join
#'
#' anti <- v %>% anti_join(df)
#'
#' anti
#'
#' autoplot(anti, aes(fill = iso2)) + ggtitle("Anti Join")
#'
semi_join.SpatVector <- function(x, y, by = NULL, copy = FALSE, ...) {
  # Use sf method
  sf_obj <- sf::st_as_sf(x)

  if (inherits(y, "SpatVector")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`SpatVector`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, y)`")
    ))
  }

  if (inherits(y, "sf")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`sf`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, terra::vect(y))`")
    ))
  }
  y <- as.data.frame(y)

  joined <- dplyr::semi_join(sf_obj,
    y = y, by = by, copy = copy, ...
  )

  return(terra::vect(joined))
}

#' @export
dplyr::semi_join

#' @importFrom dplyr anti_join
#' @export
#' @name filter-joins.SpatVector
anti_join.SpatVector <- function(x, y, by = NULL, copy = FALSE, ...) {
  # Use sf method
  sf_obj <- sf::st_as_sf(x)

  if (inherits(y, "SpatVector")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`SpatVector`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, y)`")
    ))
  }

  if (inherits(y, "sf")) {
    cli::cli_abort(paste0(
      cli::col_blue("y"),
      " should not have class ",
      cli::col_blue("`sf`"),
      ". For spatial_joins use ",
      cli::col_blue("`terra::intersect(x, terra::vect(y))`")
    ))
  }

  y <- as.data.frame(y)
  joined <- dplyr::anti_join(sf_obj,
    y = y, by = by, copy = copy, ...
  )

  return(terra::vect(joined))
}

#' @export
dplyr::anti_join
