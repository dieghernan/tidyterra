#' Bind multiple `SpatVector`, `sf` and data frame objects by column
#'
#' @description
#' Bind any number of `SpatVector`, data frames and `sf` objects by column,
#' making a wider result. This is similar to `do.call(cbind, data_frames)`.
#'
#' Where possible prefer using a [join][mutate-joins.SpatVector] to
#' combine `SpatVector` and data frame objects. `bind_spat_cols()`
#' binds the rows in order in which they appear so it is easy to create
#' meaningless results without realizing it.
#'
#' @export
#' @encoding UTF-8
#' @rdname bind_cols.SpatVector
#' @name bind_cols.SpatVector
#'
#' @seealso [dplyr::bind_cols()]
#'
#' @family dplyr.pairs
#' @family dplyr.methods
#'
#' @inheritParams dplyr::bind_cols
#'
#' @param ... Objects to combine. The first argument must be a `SpatVector`.
#'   Each subsequent argument can be a `SpatVector`, `sf` object or data frame.
#'   Inputs are [recycled][vctrs::theory-faq-recycling] to the same length, then
#'   matched by position.
#'
#' @returns A `SpatVector` with the corresponding columns. The geometry and CRS
#' correspond to the first `SpatVector` of `...`.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' `cbind()` method
#'
#' @section Methods:
#'
#' Implementation of the [dplyr::bind_cols()] function for `SpatVector`
#' objects. For the second and subsequent arguments in `...`, the geometry is
#' not `cbind`ed and only the data frame-like columns are kept.
#'
#' @examples
#' library(terra)
#' sv <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' df2 <- data.frame(letters = letters[seq_len(nrow(sv))])
#'
#' # Data frame
#' bind_spat_cols(sv, df2)
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
#' # Row sizes must be compatible when column-binding.
#' try(bind_spat_cols(sv, sfobj))
bind_spat_cols <- function(
  ...,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
) {
  dots <- rlang::list2(...)

  # Return an empty object when there are no inputs.
  if (length(dots) == 0) {
    return(terra::vect("MULTIPOINT EMPTY"))
  }

  # Support a single list of inputs.
  if (length(dots) == 1 && is.list(dots[[1]])) {
    # Unlist the first level.
    dots <- dots[[1]]
  }

  # Ensure the first input is a `SpatVector`.
  if (!inherits(dots[[1]], "SpatVector")) {
    cli::cli_abort(paste(
      "Object {.val {1}} in {.arg ...} is not a {.cls SpatVector}."
    ))
  }

  # Keep the first input as the reconstruction template.
  template <- dots[[1]]

  # Convert inputs to tibbles where needed.
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

  endobj <- dplyr::bind_cols(alltibbs, .name_repair = .name_repair)

  # Use `terra::cbind()` when the first input only has geometry.
  if (dim(template)[2] == 0) {
    vend <- cbind(template, endobj[, -1])
  } else {
    vend <- cbind(template[, 0], endobj)
  }

  # Restore grouping metadata.
  vend <- group_prepare_spat(vend, endobj)

  vend
}
