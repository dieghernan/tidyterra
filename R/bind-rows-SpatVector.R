#' Bind multiple SpatVectors and data frames by row
#'
#' @description
#' Bind any number of SpatVector, data frames and sf object by row, making a
#' longer result. This is similar to `do.call(rbind, dfs)`, but the output
#' will contain all columns that appear in any of the inputs.
#'
#' @param ... SpatVector to combine. The first argument should be a SpatVector
#'  and each of the subsequent arguments can either be a SpatVector, a sf object
#'  or a data frame. Columns are matched by name, and any missing columns will
#'  be filled with `NA`.
#' @param .id The name of an optional identifier column. Provide a string to
#'   create an output column that identifies each input. The column will use
#'   names if available, otherwise it will use positions.
#'
#' @return A SpatVector of the same type as the first element of `...`.
#' @aliases bind.Spat
#' @export
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @rdname bind_rows.SpatVector
#' @name bind_rows.SpatVector
#'
#' @seealso [dplyr::bind_rows()]
#'
#' @section terra equivalent:
#'
#' `rbind()` method
#'
#' @section Methods:
#'
#' Implementation of the [dplyr::bind_rows()] function for
#' SpatVectors.
#'
#' The first element of `...` should be a SpatVector. Subsequent elements may
#' be SpatVector, sf/sfc objects or data frames:
#'  - If subsequent SpatVector/sf/sfc present a different CRS than the first
#'    element, those elements would be reprojected to the CRS of the first
#'    element with a message.
#'  - If any element of `...` is a tibble/data frame the rows would be
#'    `cbind`ed with empty geometries with a message.
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
#' # When you supply a column name with the `.id` argument, a new
#' # column is created to link each row to its original data frame
#' bind_spat_rows(v1, v2, .id = "id")
#'
#' # Use with sf
#' sfobj <- sf::st_as_sf(v2[1, ])
#'
#' sfobj
#'
#' bind_spat_rows(v1, sfobj)
#'
#' # Would reproject with a message on different CRS
#' sfobj_3857 <- as_spatvector(sfobj) %>% project("EPSG:3857")
#'
#' bind_spat_rows(v1, sfobj_3857)
#'
#' # And with data frames with a message
#' data("mtcars")
#' bind_spat_rows(v1, sfobj, mtcars, .id = "id2")
#'
#'
#' # Use lists
#' bind_spat_rows(list(v1[1, ], sfobj[1:2, ]))
#'
#' # Or named list combined with .id
#' bind_spat_rows(list(
#'   SpatVector = v1[1, ], sf = sfobj[1, ],
#'   mtcars = mtcars[1, ]
#' ), .id = "source")
bind_spat_rows <- function(..., .id = NULL) {
  dots <- rlang::list2(...)
  # Return empty on none
  if (length(dots) == 0) {
    return(terra::vect("POINT EMPTY"))
  }

  # Make it work with list
  if (length(dots) == 1 && is.list(dots[[1]])) {
    # If is a list unlist the first level
    dots <- dots[[1]]
  }
  named_list <- as.character(seq_len(length(dots)))

  # Named lists
  if (!is.null(names(dots))) {
    maybe_names <- names(dots)
    maybe_names <- maybe_names[maybe_names != ""]
    maybe_names <- maybe_names[!is.na(maybe_names)]
    if (length(maybe_names) == length(named_list)) {
      named_list <- as.character(maybe_names)
    }
  }


  # Checks
  # Ensure first is SpatVector
  if (!inherits(dots[[1]], "SpatVector")) {
    cli::cli_abort(paste(
      "Object #1 in",
      cli::col_blue("..."), "is not a",
      cli::col_blue("SpatVector")
    ))
  }

  # Get templates
  template <- dots[[1]]

  # Ensure all are SpatVectors and add ids if required
  allspatvect <- lapply(seq_len(length(dots)), function(i) {
    x <- dots[[i]]

    if (inherits(x, c("SpatVector", "sf", "sfc"))) {
      x <- crs_compare(x, template, i)
      return(x)
    }

    # If tibble convert (internally) to SpatVector
    # Rest as tibble
    if (!inherits(x, "data.frame")) {
      cli::cli_abort(paste0(
        cli::style_bold("In bind_spat_rows(): "),
        "Object #", i, " in ", cli::col_blue("..."),
        " is not a data.frame/tbl"
      ))
    }

    cli::cli_alert_warning(paste0(
      cli::style_bold(
        "Object #", i, " in ", cli::col_blue("..."),
        " is a data.frame/tbl."
      ),
      cli::col_grey("\nThe result would present empty geoms")
    ))

    x <- as_tibble(x)
    x$geometry <- NA

    attr(x, "source") <- "SpatVector"
    attr(x, "crs") <- terra::crs(template)
    attr(x, "geomtype") <- terra::geomtype(template)

    as_spat_internal(x)
  })
  vend <- do.call("rbind", allspatvect)

  # Adjust NAs
  df <- as_tibble(vend)
  df[is.na(df)] <- NA

  vend <- cbind(vend[, 0], df)

  # Regen groups
  vend <- group_prepare_spat(vend, template)

  # If id not requested we are done
  if (is.null(.id)) {
    return(vend)
  }

  # Need to add a variable with id

  # Create vector of indexes identifying source of each row
  rows_vect <- unlist(lapply(allspatvect, nrow))
  theindex <- unlist(lapply(seq_len(length(rows_vect)), function(x) {
    rep(x, rows_vect[x])
  }))

  keep_names <- names(df)

  df[[.id]] <- named_list[theindex]

  # Rearrange if the id var has been added
  if (!.id %in% keep_names) {
    df <- df[, c(.id, keep_names)]
  }

  vend <- cbind(vend[, 0], df)

  vend <- group_prepare_spat(vend, template)

  vend
}

crs_compare <- function(a, b, index) {
  if (!identical(pull_crs(a), pull_crs(b))) {
    cli::cli_alert_warning(
      paste0(
        cli::style_bold("Reprojecting object #", index),
        cli::col_grey(
          "\nObject #", index, " in ", cli::col_blue("..."),
          " doesn't have the same CRS than object #1"
        )
      )
    )
  }

  if (inherits(a, c("sf", "sfc"))) a <- as_spatvector(a)

  if (is.na(pull_crs(b))) {
    terra::crs(a) <- pull_crs(b)
  } else {
    a <- terra::project(a, pull_crs(b))
  }

  return(a)
}
