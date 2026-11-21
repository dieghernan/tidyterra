#' Keep distinct/unique rows and geometries of [`SpatVector`][terra::vect]
#'   objects
#'
#' @description
#' Keep only unique/distinct rows and geometries from a
#'   [`SpatVector`][terra::vect].
#'
#' @rdname distinct.SpatVector
#' @name distinct.SpatVector
#'
#' @seealso [dplyr::distinct()], [terra::unique()].
#'
#' @family dplyr.rows
#'
#' @importFrom dplyr distinct
#'
#' @inheritParams arrange.SpatVector .data
#' @inheritParams dplyr::distinct
#' @param ... <[`data-masking`][rlang::args_data_masking]> Optional variables or
#'   expressions used to determine uniqueness, such as `parity = id %% 2`.
#'   If multiple rows share the same values, only the first is kept. If omitted,
#'   all variables are used. Pass the reserved name `geometry` to compare
#'   geometries. See **Methods**.
#'
#' @returns A [`SpatVector`][terra::vect] object.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::unique()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::distinct()] method for
#' [`SpatVector`][terra::vect] objects.
#'
#' You can remove duplicate geometries by passing the reserved name
#' `geometry` to `...`. See **Examples**.
#'
#' @encoding UTF-8
#' @export
#' @examples
#'
#' library(terra)
#'
#' v <- vect(system.file("ex/lux.shp", package = "terra"))
#'
#' # Create a vector with duplicates.
#' v <- v[sample(seq_len(nrow(v)), 100, replace = TRUE), ]
#' v$gr <- sample(LETTERS[1:3], 100, replace = TRUE)
#'
#' # All duplicates
#' ex1 <- distinct(v)
#' ex1
#'
#' nrow(ex1)
#'
#' # Duplicates by gr
#' ex2 <- distinct(v, gr)
#' ex2
#' nrow(ex2)
#'
#' # Keep all columns.
#' ex2b <- distinct(v, gr, .keep_all = TRUE)
#' ex2b
#' nrow(ex2b)
#'
#' # Unique geometries
#' ex3 <- distinct(v, geometry)
#'
#' ex3
#' nrow(ex3)
#' # Same as terra::unique()
#' terra::unique(ex3)
#'
#' # Keep information for unique values.
#' distinct(v, geometry, .keep_all = TRUE)
distinct.SpatVector <- function(.data, ..., .keep_all = FALSE) {
  dots <- rlang::enquos(...)
  key_labels <- vapply(dots, rlang::as_label, character(1))
  if (length(dots) == 0L || "geometry" %in% key_labels) {
    a_tbl <- as_tbl_internal(.data)
    dist <- dplyr::distinct(a_tbl, !!!dots, .keep_all = .keep_all)

    # Regenerate the `SpatVector`.
    distin <- restore_attr(dist, a_tbl)
    vend <- as_spat_internal(distin)
    vend <- group_prepare_spat(vend, dist)

    return(vend)
  }

  a_tbl <- as_tibble(.data)
  # As in dplyr, compute keys across the input before comparing within groups.
  keys <- dplyr::transmute(dplyr::ungroup(a_tbl), !!!dots)
  key_names <- union(
    setdiff(dplyr::group_vars(a_tbl), names(keys)),
    names(keys)
  )
  a_tbl <- dplyr::dplyr_col_modify(a_tbl, as.list(keys))
  row_id <- vctrs::vec_unique_loc(a_tbl[key_names])

  keep <- key_names
  if (isTRUE(.keep_all)) {
    keep <- union(keep, names(a_tbl))
  }
  dist <- dplyr::dplyr_row_slice(a_tbl[keep], row_id)

  vend <- cbind(.data[row_id, 0], dist)

  # Restore groups.
  vend <- group_prepare_spat(vend, dist)

  vend
}

#' @export
dplyr::distinct
