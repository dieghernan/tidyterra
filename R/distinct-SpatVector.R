#' Keep distinct/unique rows and geometries of SpatVector objects
#'
#' @description
#' Keep only unique/distinct rows and geometries from a SpatVector.
#'
#' @export
#' @rdname distinct.SpatVector
#' @name distinct.SpatVector
#'
#' @seealso [dplyr::distinct()], [terra::unique()]
#'
#' @family dplyr.rows
#' @family dplyr.methods
#'
#' @importFrom dplyr distinct
#'
#' @param .data A SpatVector created with [terra::vect()].
#' @param ... <[`data-masking`][dplyr::distinct]> Optional variables to
#'   use when determining uniqueness. If there are multiple rows for a given
#'   combination of inputs, only the first row will be preserved. If omitted,
#'   will use all variables in the data frame. There is a reserved variable
#'   name, `geometry`, that would remove duplicate geometries. See **Methods**.
#' @inheritParams dplyr::distinct
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' [terra::unique()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::distinct()] function.
#'
#' ## SpatVector
#'
#' It is possible to remove duplicate geometries including the `geometry`
#' variable explicitly in the `...` call. See **Examples**.
#'
#'
#' @examples
#'
#' library(terra)
#'
#' v <- vect(system.file("ex/lux.shp", package = "terra"))
#'
#' # Create a vector with dups
#' v <- v[sample(seq_len(nrow(v)), 100, replace = TRUE), ]
#' v$gr <- sample(LETTERS[1:3], 100, replace = TRUE)
#'
#' # All duplicates
#' ex1 <- distinct(v)
#' ex1
#'
#' nrow(ex1)
#'
#' # Duplicates by NAME_1
#' ex2 <- distinct(v, gr)
#' ex2
#' nrow(ex2)
#'
#' # Same but keeping all cols
#' ex2b <- distinct(v, gr, .keep_all = TRUE)
#' ex2b
#' nrow(ex2b)
#'
#'
#' # Unique geometries
#' ex3 <- distinct(v, geometry)
#'
#' ex3
#' nrow(ex3)
#' # Same as terra::unique()
#' terra::unique(ex3)
#'
#' # Unique keeping info
#' distinct(v, geometry, .keep_all = TRUE)
distinct.SpatVector <- function(.data, ..., .keep_all = FALSE) {
  # Own implementation
  a_tbl <- as_tbl_spatvect_attr(.data)

  if (rlang::dots_n(...) == 0) {
    dist <- dplyr::distinct(a_tbl, ..., .keep_all = TRUE)

    # tidyterra attributes are dropped, regenerate...
    attr(dist, "source") <- attr(a_tbl, "source")
    attr(dist, "crs") <- attr(a_tbl, "crs")
    attr(dist, "geomtype") <- attr(a_tbl, "geomtype")

    return(as_spatvect_attr(dist))
  }

  # Renaming based on conversion to tibble
  names(.data) <- names(a_tbl)[seq_len(ncol(.data))]

  # Add a index for identifying rows to extract
  a_tbl$tterra_index <- seq_len(nrow(a_tbl))

  # Get dots via select
  dots_labs <- names(select(a_tbl[1, ], ...))

  topass <- c("tterra_index", dots_labs)

  dist <- distinct(a_tbl[, topass], ..., .keep_all = TRUE)

  # And return using indexes for subsetting
  # colnames
  n <- names(.data)
  if (.keep_all == FALSE) {
    n <- n[n %in% names(dist)]
  }
  # Row index
  r <- as.integer(dist$tterra_index)
  dist_v <- .data[r, n]

  # Ensure groups
  if (dplyr::is_grouped_df(dist)) {
    attr(dist_v, "group_vars") <- dplyr::group_vars(dist)
  } else {
    attr(dist_v, "group_vars") <- NULL
  }

  return(dist_v)
}



#' @export
dplyr::distinct
