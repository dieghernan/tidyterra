#' Change layer/attribute order
#'
#' @description
#'
#' Use `relocate()` to change layer/attribute positions, using the same syntax
#' as [select()] to make it easy to move blocks of layers/attributes at once.
#'
#' @export
#' @rdname relocate.Spat
#' @name relocate.Spat
#'
#' @inheritParams select.Spat
#' @param ... [`tidy-select`][dplyr::relocate] layers/attributes to move.
#'
#' @param .before,.after [`tidy-select`][dplyr::relocate] Destination of
#'   layers/attributes selected by `...`. Supplying neither will move
#'   layers/attributes to the left-hand side; specifying both is an error.
#'
#' @return A Spat* object  of the same class than `.data`. See **Methods**.
#'
#' @seealso [dplyr::relocate()]
#'
#' @family dplyr.cols
#' @family dplyr.methods
#'
#' @importFrom dplyr relocate
#'
#' @section terra equivalent:
#'
#' `terra::subset(data, c("name_layer", "name_other_layer"))`
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::relocate()] function.
#'
#' ## SpatRaster
#'
#' Relocate layers of a SpatRaster.
#'
#' ## SpatVector
#'
#' The result is a SpatVector with the attributes on a different order.
#'
#' @examples
#'
#' library(terra)
#'
#'
#' f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
#' spatrast <- rast(f) %>% mutate(aa = 1, bb = 2, cc = 3)
#'
#' names(spatrast)
#'
#'
#' spatrast %>%
#'   relocate(bb, .before = cyl_tile_3) %>%
#'   relocate(cyl_tile_1, .after = last_col())
#'
relocate.SpatRaster <- function(.data, ..., .before = NULL, .after = NULL) {
  # With template
  df <- .data[1]

  values_relocated <- dplyr::relocate(df, ...,
    .before = {{ .before }},
    .after = {{ .after }}
  )


  finalrast <- .data
  finalrast <- terra::subset(finalrast, names(values_relocated))

  return(finalrast)
}


#' @rdname relocate.Spat
#' @export
relocate.SpatVector <- function(.data, ..., .before = NULL, .after = NULL) {
  # Use own method
  # With template
  df <- as_tibble(.data[1, ])

  values_relocated <- dplyr::relocate(df, ...,
    .before = {{ .before }},
    .after = {{ .after }}
  )

  vend <- .data[, names(values_relocated)]
  vend <- group_prepare_spat(vend, values_relocated)

  return(vend)
}

#' @export
dplyr::relocate
