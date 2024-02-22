#' Keep distinct/unique rows and geometries of `SpatVector` objects
#'
#' @description
#' Keep only unique/distinct rows and geometries from a `SpatVector`.
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
#' @param .data A `SpatVector` created with [terra::vect()].
#' @param ... <[`data-masking`][rlang::args_data_masking]> Optional variables to
#'   use when determining uniqueness. If there are multiple rows for a given
#'   combination of inputs, only the first row will be preserved. If omitted,
#'   will use all variables in the data frame. There is a reserved variable
#'   name, `geometry`, that would remove duplicate geometries. See **Methods**.
#' @inheritParams dplyr::distinct
#' @return A `SpatVector` object.
#'
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::unique()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::distinct()] function.
#'
#' ## `SpatVector`
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
  # Identify if geometry is called on dots
  # Get dots via select
  dots_labs <- names(select(as_tbl_internal(.data[1, ]), ...))


  # If call is empty or geomtry included, use internal
  # since we need geometry column
  if (rlang::dots_n(...) == 0 || "geometry" %in% dots_labs) {
    a_tbl <- as_tbl_internal(.data)
    dist <- dplyr::distinct(a_tbl, ..., .keep_all = .keep_all)

    # Regenerate
    distin <- restore_attr(dist, a_tbl)
    vend <- as_spat_internal(distin)
    vend <- group_prepare_spat(vend, dist)

    return(vend)
  }

  # If not use indexes on regular tibble
  a_tbl <- as_tibble(.data)
  keepcopy <- a_tbl

  # Add a index for identifying rows to extract
  index_var <- make_safe_index("tterra_index", a_tbl)

  a_tbl[[index_var]] <- seq_len(nrow(a_tbl))

  topass <- c(index_var, dots_labs)

  dist <- distinct(a_tbl[, topass], ..., .keep_all = TRUE)

  # And return using indexes for subsetting
  row_id <- as.integer(dist[[index_var]])

  dist <- dist[, names(dist) != index_var]

  # Add rest of columns if requested
  if (isTRUE(.keep_all)) {
    misscol <- keepcopy[row_id, !names(keepcopy) %in% dots_labs]
    dist <- dplyr::bind_cols(dist, misscol)
  }

  vend <- cbind(.data[row_id, 0], dist)

  # Ensure groups
  vend <- group_prepare_spat(vend, dist)


  return(vend)
}



#' @export
dplyr::distinct
