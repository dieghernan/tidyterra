#' Unite `Spat*` layers or attributes
#'
#' @description
#'
#' `unite()` combines multiple layers or attributes by pasting their values
#' together.
#'
#' @export
#' @encoding UTF-8
#' @rdname unite.Spat
#' @name unite.Spat
#'
#' @seealso [tidyr::unite()]
#'
#' @family tidyr.character
#' @family tidyr.methods
#'
#' @importFrom tidyr unite
#'
#' @inheritParams tidyr::unite
#'
#' @param data A `SpatRaster` or `SpatVector`.
#' @returns A `SpatRaster` or `SpatVector` object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::unite()] method.
#'
#' ## `SpatRaster`
#'
#' The selected layers are united cell by cell. The new layer is categorical
#' because [tidyr::unite()] returns a character vector.
#'
#' ## `SpatVector`
#'
#' The geometry column has sticky behavior and is never united with attributes.
#'
#' @examples
#' library(tidyr)
#'
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' unite(v, "label", iso2, cpro, sep = "-")
#'
#' r <- terra::rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))
#'
#' unite(r, "label", tavg_04, tavg_05, sep = "-", remove = FALSE)
#'
unite.SpatRaster <- function(
  data,
  col,
  ...,
  sep = "_",
  remove = TRUE,
  na.rm = FALSE
) {
  tbl <- as_tbl_internal(data)
  xy <- dplyr::select(tbl, 1:2)
  values <- dplyr::select(tbl, -c(1, 2))

  united <- tidyr::unite(
    values,
    {{ col }},
    ...,
    sep = sep,
    remove = remove,
    na.rm = na.rm
  )

  final_tbl <- dplyr::bind_cols(xy, united)
  final_tbl <- data.table::as.data.table(final_tbl)
  final_tbl <- restore_attr(final_tbl, tbl)

  dims <- attributes(tbl)$dims
  dims[3] <- ncol(united)
  attr(final_tbl, "dims") <- dims

  as_spat_internal(final_tbl)
}

#' @export
#' @encoding UTF-8
#' @rdname unite.Spat
unite.SpatVector <- function(
  data,
  col,
  ...,
  sep = "_",
  remove = TRUE,
  na.rm = FALSE
) {
  tbl <- as_tbl_internal(data)

  tmpl <- dplyr::ungroup(tbl[1, ])
  cols_char <- remove_geom_col(tmpl, c(...), "...")

  united <- tidyr::unite(
    tbl,
    {{ col }},
    dplyr::all_of(cols_char),
    sep = sep,
    remove = remove,
    na.rm = na.rm
  )

  as_spat_internal(restore_attr(united, tbl))
}

#' @export
tidyr::unite
