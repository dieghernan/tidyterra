#' Create, modify, and delete cell values/layers/attributes of `Spat*` objects
#'
#' @description
#'
#' `mutate()` adds new layers/attributes and preserves existing ones on a
#' `Spat*` object. `transmute()` adds new layers/attributes and drops existing
#' ones. New variables overwrite existing variables of the same name. Variables
#' can be removed by setting their value to `NULL`.
#'
#'
#' @return A `Spat*` object  of the same class than `.data`. See **Methods**.
#'
#' @inheritParams select.Spat
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs.
#'   The name gives the name of the layer/attribute in the output.
#'
#' @export
#' @rdname mutate.Spat
#' @name mutate.Spat
#'
#' @aliases transmute.Spat
#'
#' @importFrom dplyr mutate
#'
#' @seealso
#'
#' [dplyr::mutate()], [dplyr::transmute()] methods.
#'
#' \CRANpkg{terra} provides several ways to modify `Spat*` objects:
#'
#' - [terra::ifel()].
#' - [terra::classify()].
#' - [terra::clamp()].
#' - [terra::app()], [terra::lapp()], [terra::tapp()].
#'
#' @family single table verbs
#' @family dplyr.cols
#' @family dplyr.methods
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' Some \CRANpkg{terra} methods for modifying cell values:
#' [terra::ifel()], [terra::classify()], [terra::clamp()], [terra::app()],
#' [terra::lapp()], [terra::tapp()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::mutate()], [dplyr::transmute()]
#' functions.
#'
#' ## `SpatRaster`
#'
#' Add new layers and preserves existing ones. The result is a
#' `SpatRaster` with the same extent, resolution and crs than `.data`. Only the
#' values (and possibly the number) of layers is modified.
#'
#' `transmute()` would keep only the layers created with `...`.
#'
#' ## `SpatVector`
#'
#' The result is a `SpatVector` with the modified (and possibly renamed)
#' attributes on the function call.
#'
#' `transmute()` would keep only the attributes created with `...`.
#'
#' @examples
#'
#' library(terra)
#'
#' # SpatRaster method
#' f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
#' spatrast <- rast(f)
#'
#' mod <- spatrast %>%
#'   mutate(exp_lyr1 = exp(tavg_04 / 10)) %>%
#'   select(tavg_04, exp_lyr1)
#'
#' mod
#' plot(mod)
#'
#' # SpatVector method
#' f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#' v <- vect(f)
#'
#' v %>%
#'   mutate(cpro2 = paste0(cpro, "-CyL")) %>%
#'   select(cpro, cpro2)
mutate.SpatRaster <- function(.data, ...) {
  df <- as_tbl_internal(.data)

  xy <- dplyr::select(df, 1:2)

  values <- dplyr::select(df, -c(1, 2))

  values_mutate <- dplyr::mutate(values, ...)

  # dtplyr
  xy <- data.table::as.data.table(xy)
  values_mutate <- data.table::as.data.table(values_mutate)

  final_df <- dplyr::bind_cols(xy, values_mutate)

  # To data.table and rearrange attrs
  final_df <- data.table::as.data.table(final_df)

  # Spatial attrs
  init_att <- attributes(df)
  final_att <- attributes(final_df)

  spat_attrs <- init_att[setdiff(names(init_att), names(final_att))]

  attributes(final_df) <- c(final_att, spat_attrs)

  # Rearrange number of layers
  dims <- attributes(df)$dims
  dims[3] <- ncol(values_mutate)
  attr(final_df, "dims") <- dims

  final_rast <- as_spat_internal(final_df)

  if (any(terra::has.colors(.data))) {
    ctab_list <- terra::coltab(.data)

    # Assign coltab by layer
    l2 <- lapply(seq_len(terra::nlyr(final_rast)), function(x) {
      rr <- terra::subset(final_rast, x)
      if (x <= length(ctab_list)) {
        ctab <- ctab_list[x]
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
#' @rdname mutate.Spat
mutate.SpatVector <- function(.data, ...) {
  # Use own method
  tbl <- as_tibble(.data)
  mutated <- dplyr::mutate(tbl, ...)

  # Bind
  vend <- cbind(.data[, 0], mutated)

  # Prepare groups
  vend <- group_prepare_spat(vend, mutated)

  vend
}
#' @export
#' @rdname mutate.Spat
#' @importFrom dplyr transmute
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
#' @rdname mutate.Spat
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
dplyr::mutate

#' @export
dplyr::transmute
