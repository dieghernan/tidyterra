#' Bind multiple `SpatVector`, `sf/sfc` and data frame objects by row
#'
#' @description
#' Bind any number of `SpatVector`, data frames and `sf/sfc` objects by row,
#' making a longer result. This is similar to `do.call(rbind, data_frames)`,
#' but the output will contain all columns that appear in any of the inputs.
#'
#' @export
#' @encoding UTF-8
#'
#' @rdname bind_rows.SpatVector
#' @name bind_rows.SpatVector
#'
#' @aliases bind.Spat
#' @seealso [dplyr::bind_rows()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @inheritParams dplyr::bind_rows
#'
#' @param ... Objects to combine. The first argument must be a `SpatVector`.
#'   Each subsequent argument can be a `SpatVector`, `sf/sfc` object or data
#'   frame. Columns are matched by name and any missing columns are filled with
#'   `NA`.
#' @returns A `SpatVector` of the same type as the first element of `...`.
#' @section \CRANpkg{terra} equivalent:
#'
#' `rbind()` method
#'
#' @section Methods:
#'
#' Implementation of the [dplyr::bind_rows()] function for
#' `SpatVector` objects.
#'
#' The first argument should be a `SpatVector`. Each subsequent argument can be
#' a `SpatVector`, `sf/sfc` object or data frame:
#'
#' - If subsequent `SpatVector/sf/sfc` objects have a different CRS than the
#'   first element, those elements are reprojected to the CRS of the
#'   first element with a message.
#' - If any element of `...` is a tibble/data frame, the rows are column-bound
#'   with empty geometries with a message.
#'
#' @examples
#'
#' library(terra)
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' v1 <- v[1, "cpro"]
#' v2 <- v[3:5, c("name", "iso2")]
#'
#' # You can supply individual SpatVector as arguments:
#' bind_spat_rows(v1, v2)
#'
#' # When you supply a column name with the `.id` argument, a new column is
#' # created to link each row to its original data frame.
#' bind_spat_rows(v1, v2, .id = "id")
#'
#' \donttest{
#' # Use with sf
#' sfobj <- sf::st_as_sf(v2[1, ])
#'
#' sfobj
#'
#' bind_spat_rows(v1, sfobj)
#'
#' # Would reproject with a message on different CRS
#' sfobj_3857 <- as_spatvector(sfobj) |> project("EPSG:3857")
#'
#' bind_spat_rows(v1, sfobj_3857)
#'
#' # And with data frames with a message
#' data("mtcars")
#' bind_spat_rows(v1, sfobj, mtcars, .id = "id2")
#'
#' # Use lists
#' bind_spat_rows(list(v1[1, ], sfobj[1:2, ]))
#'
#' # Or named list combined with .id
#' bind_spat_rows(list(
#'   SpatVector = v1[1, ], sf = sfobj[1, ],
#'   mtcars = mtcars[1, ]
#' ), .id = "source")
#' }
bind_spat_rows <- function(..., .id = NULL) {
  dots <- rlang::list2(...)
  # Return an empty object when no inputs are supplied.
  if (length(dots) == 0) {
    return(terra::vect("MULTIPOINT EMPTY"))
  }

  # Support list input.
  if (length(dots) == 1 && is.list(dots[[1]])) {
    # Unlist the first level.
    dots <- dots[[1]]
  }

  named_list <- as.character(seq_along(dots))

  # Preserve names from named lists.
  if (!is.null(names(dots))) {
    maybe_names <- names(dots)
    maybe_names <- maybe_names[nzchar(maybe_names)]
    maybe_names <- maybe_names[!is.na(maybe_names)]
    if (length(maybe_names) == length(named_list)) {
      named_list <- as.character(maybe_names)
    }
  }

  # Ensure the first input is a `SpatVector`.
  if (!inherits(dots[[1]], "SpatVector")) {
    cli::cli_abort(paste(
      "Object {.val {1}} in {.arg ...} is not a {.cls SpatVector}."
    ))
  }

  # Get the template.
  template <- dots[[1]]

  # Convert all inputs to tibbles first.
  alltibbs <- lapply(seq_along(dots), function(i) {
    x <- dots[[i]]

    # The first input is always a `SpatVector`.
    if (i == 1) {
      frst <- as_tibble(x)

      # If first is only geometry, add a mock variable.
      if (nrow(frst) == 0) {
        frst <- tibble::tibble(first_empty = seq_len(nrow(x)))
      }

      return(frst)
    }

    # Remaining cases.

    if (inherits(x, "SpatVector")) {
      return(as_tibble(x))
    }

    if (inherits(x, "sf")) {
      return(sf::st_drop_geometry(x))
    }

    x
  })

  # Get all geometries.
  # Ensure all are SpatVectors and add ids if required.
  allspatvect <- lapply(seq_along(dots), function(i) {
    x <- dots[[i]]

    if (inherits(x, c("SpatVector", "sf", "sfc"))) {
      x <- crs_compare(x, template, i)
      return(x[, 0])
    }

    # Convert tibbles internally to SpatVector.
    # Keep the rest as tibble.
    if (!inherits(x, "data.frame")) {
      cli::cli_abort(paste(
        "In {.fun tidyterra::bind_spat_rows}:",
        "object {.val {i}} in {.arg ...} is not a {.cls data.frame}."
      ))
    }

    cli::cli_alert_warning(paste(
      "Object {.val {i}} in {.arg ...} is {.cls {class(x)}}",
      cli::col_grey("\nThe result includes empty geometries.")
    ))

    x <- as_tibble(x)
    x$geometry <- NA

    attr(x, "source") <- "SpatVector"
    attr(x, "crs") <- terra::crs(template)
    attr(x, "geomtype") <- terra::geomtype(template)

    as_spat_internal(x)[, 0]
  })

  # Get geometries.
  vend <- do.call("rbind", allspatvect)

  # Bind rows.
  if (length(named_list) == length(alltibbs)) {
    names(alltibbs) <- named_list
  }

  binded <- dplyr::bind_rows(alltibbs, .id = .id)
  vend <- cbind(vend[, 0], binded)

  # Regenerate groups.
  vend <- group_prepare_spat(vend, binded)

  vend
}

crs_compare <- function(a, b, index) {
  if (!identical(pull_crs(a), pull_crs(b))) {
    cli::cli_alert_warning(paste0(
      "Reprojecting object {.val {index}} in {.arg ...} because it",
      " does not have the same CRS as object {.val {1}}."
    ))
  }

  if (inherits(a, c("sf", "sfc"))) {
    a <- as_spatvector(a)
  }

  if (is.na(pull_crs(b))) {
    terra::crs(a) <- pull_crs(b)
  } else {
    a <- terra::project(a, pull_crs(b))
  }

  a
}
