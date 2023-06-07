#' Bind multiple SpatVectors and data frames by column
#'
#' @description
#' Bind any number of SpatVector, data frames and sf object by column, making a
#' wider result. This is similar to `do.call(cbind, dfs)`.
#'
#' Where possible prefer using a [join][mutate-joins.SpatVector] to combine
#' SpatVectors and data frames. `bind_spat_cols()` binds the rows in order in
#' which they appear so it is easy to create meaningless results without
#' realizing it.
#'
#' @param ... SpatVector to combine. The first argument should be a SpatVector
#'  and each of the subsequent arguments can either be a SpatVector, a sf object
#'  or a data frame. Inputs are [recycled][dplyr::bind_cols()] to the same
#'  length, then matched by position.
#' @param .name_repair One of `"unique"`, `"universal"`, or `"check_unique"`.
#'   See [dplyr::bind_cols()] for Details.
#' @return A SpatVector with the corresponding cols. The geometry and CRS
#' would correspond to the the first SpatVector of `...`.
#' @export
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @rdname bind_cols.SpatVector
#' @name bind_cols.SpatVector
#'
#' @seealso [dplyr::bind_cols()]
#'
#' @section terra equivalent:
#'
#' `cbind()` method
#'
#' @section Methods:
#'
#' Implementation of the [dplyr::bind_rows()] function for
#' SpatVectors. Note that for the second and subsequent arguments on `...` the
#' geometry would not be `cbind`ed, and only the data frame (-ish) columns
#' would be kept.
#'
#' @examples
#' library(terra)
#' sv <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' df2 <- data.frame(letters = letters[seq_len(nrow(sv))])
#'
#' # Data frame
#' bind_spat_cols(sv, df2)
#'
#'
#' # Another SpatVector
#' bind_spat_cols(sv[1:2, ], sv[3:4, ])
#'
#' # sf objects
#' sfobj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
#'
#' bind_spat_cols(sv[1:9, ], sfobj[1:9, ])
#'
#' # Mixed
#'
#' end <- bind_spat_cols(sv, sfobj[seq_len(nrow(sv)), 1:2], df2)
#'
#' end
#' glimpse(end)
#'
#' # Row sizes must be compatible when column-binding
#' try(bind_spat_cols(sv, sfobj))
bind_spat_cols <- function(...,
                           .name_repair = c(
                             "unique", "universal",
                             "check_unique", "minimal"
                           )) {
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

  # Checks
  # Ensure first is SpatVector
  if (!inherits(dots[[1]], "SpatVector")) {
    cli::cli_abort(paste(
      "Object 1 in {.arg ...} is not a {.cls SpatVector}"
    ))
  }

  # Get templates
  template <- dots[[1]]

  # Ensure all are tibbles
  alltibbs <- lapply(seq_len(length(dots)), function(i) {
    x <- dots[[i]]

    # First is always a SpatVector
    if (i == 1) {
      frst <- as_tibble(x)

      # Case when first is only geometry, need to add a mock var
      if (nrow(frst) == 0) {
        frst <- tibble::tibble(first_empty = seq_len(nrow(x)))
      }

      return(frst)
    }

    # Rest of cases

    if (inherits(x, "SpatVector")) {
      return(as_tibble(x))
    }


    if (inherits(x, "sf")) {
      return(sf::st_drop_geometry(x))
    }

    return(x)
  })


  endobj <- dplyr::bind_cols(alltibbs, .name_repair = .name_repair)

  # If first was geom only bind the rest
  # Use cbind terra method
  if (dim(template)[2] == 0) {
    vend <- cbind(template, endobj[, -1])
  } else {
    vend <- cbind(template[, 0], endobj)
  }


  # Groups
  vend <- group_prepare_spat(vend, endobj)

  return(vend)
}
