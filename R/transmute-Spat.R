#' Create, modify, and delete cell values/layers/attributes of `Spat*` objects
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `transmute()` creates a new object containing only the specified
#' computations. It's superseded because you can perform the same job
#' with `mutate(.keep = "none")`.
#'
#' @rdname transmute.Spat
#' @name transmute.Spat
#' @keywords internal
#'
#' @seealso
#' [`mutate.Spat`], [dplyr::transmute()] methods.
#'
#' @inheritParams mutate.Spat
#'
#' @export
#'
#' @importFrom dplyr transmute
#' @inherit mutate.Spat return
#'
#' @section Methods:
#' Implementation of the **generic** [dplyr::transmute()] method.
#'
#' @examples
#' library(terra)
#'
#' # SpatVector method
#' f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#' v <- vect(f)
#'
#' v |>
#'   transmute(cpro2 = paste0(cpro, "-CyL"))
transmute.SpatRaster <- function(.data, ...) {
  df <- as_tbl_internal(.data)

  xy <- dplyr::select(df, 1:2)

  values <- dplyr::select(df, -c(1, 2))

  values_transm <- dplyr::transmute(values, ...)

  # dtplyr
  xy <- data.table::as.data.table(xy)
  values_transm <- data.table::as.data.table(values_transm)

  final_df <- dplyr::bind_cols(xy, values_transm)

  # To data.table and rearrange attrs
  final_df <- data.table::as.data.table(final_df)

  # Spatial attrs
  init_att <- attributes(df)
  final_att <- attributes(final_df)

  spat_attrs <- init_att[setdiff(names(init_att), names(final_att))]

  attributes(final_df) <- c(final_att, spat_attrs)

  # Rearrange number of layers
  dims <- attributes(df)$dims
  dims[3] <- ncol(values_transm)
  attr(final_df, "dims") <- dims

  final_rast <- as_spat_internal(final_df)

  # Check coltab
  if (
    any(terra::has.colors(.data)) && any(names(final_rast) %in% names(.data))
  ) {
    ctab_list_init <- terra::coltab(.data)
    names(ctab_list_init) <- names(.data)
    namesend <- names(final_rast)

    ctab_list <- ctab_list_init[namesend %in% names(.data)]

    # Assign coltab by layer
    l2 <- lapply(seq_len(terra::nlyr(final_rast)), function(x) {
      rr <- terra::subset(final_rast, x)
      if (names(rr) %in% names(ctab_list)) {
        ctab <- ctab_list[match(names(rr), names(ctab_list))]
      } else {
        ctab <- NULL
      }

      terra::coltab(rr) <- ctab
      rr
    })
    final_rast <- do.call("c", l2)
  }

  final_rast
}
#' @export
#' @rdname transmute.Spat
transmute.SpatVector <- function(.data, ...) {
  # Use own method
  tbl <- as_tibble(.data)
  transm <- dplyr::transmute(tbl, ...)

  if (ncol(transm) > 0) {
    # Bind
    vend <- cbind(.data[, 0], transm)
  } else {
    vend <- .data[, 0]
  }

  # Prepare groups
  vend <- group_prepare_spat(vend, transm)

  vend
}

#' @export
dplyr::transmute
