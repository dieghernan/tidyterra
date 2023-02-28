#' Rename layers/attributes
#'
#' @description
#' `rename()` changes the names of individual layers/attributes using
#' `new_name = old_name` syntax; `rename_with()` renames layers/attributes
#' using a function.
#'
#' @export
#' @rdname rename.Spat
#' @name rename.Spat
#'
#' @importFrom dplyr rename
#'
#' @inheritParams select.Spat
#' @inheritParams dplyr::rename
#' @param ...
#'   For `rename()`: `tidy-select` Use `new_name = old_name to rename`
#'   selected variables.
#'
#'   For `rename_with()`: additional arguments passed onto `.fn`.
#' @param .cols	`tidy-select` Columns to rename; defaults to all columns.
#'
#' @return A Spat* object  of the same class than `.data`. See **Methods**.
#'
#' @seealso [dplyr::rename()]
#'
#' @family single table verbs
#' @family dplyr.cols
#' @family dplyr.methods
#'
#' @section terra equivalent:
#'
#' `names(Spat*) <- c("a", "b", "c")`
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::rename()] function.
#'
#' ## SpatRaster
#'
#' Rename layers of a SpatRaster.
#'
#' ## SpatVector
#'
#' This method relies on the implementation of [dplyr::rename()] method on the
#' sf package. The result is a SpatVector with the renamed attributes on the
#' function call.
#'
#' @examples
#'
#' library(terra)
#' f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
#' spatrast <- rast(f) %>% mutate(aa = 1, bb = 2, cc = 3)
#'
#' spatrast
#'
#' spatrast %>% rename(
#'   this_first = cyl_tile_1,
#'   this_second = cyl_tile_2
#' )
#'
#' spatrast %>% rename_with(
#'   toupper,
#'   .cols = starts_with("c")
#' )
rename.SpatRaster <- function(.data, ...) {
  # Use template
  df <- .data[1]
  df_rename <- dplyr::rename(df, ...)

  final_rast <- .data
  names(final_rast) <- names(df_rename)

  return(final_rast)
}

#' @rdname rename.Spat
#' @importFrom dplyr rename_with
#' @importFrom dplyr everything
#' @export
rename_with.SpatRaster <- function(.data, .fn, .cols = everything(), ...) {
  # Use template
  df <- .data[1]
  .fn <- rlang::as_function(.fn)
  .cols <- rlang::enquo(.cols)

  df_rename <- dplyr::rename_with(
    df,
    .fn,
    !!.cols,
    ...
  )

  final_rast <- .data
  names(final_rast) <- names(df_rename)

  return(final_rast)
}

#' @rdname rename.Spat
#' @export
rename.SpatVector <- function(.data, ...) {
  # Use sf
  sfobj <- dplyr::rename(sf::st_as_sf(.data), ...)

  end <- terra::vect(sfobj)

  return(end)
}

#' @rdname rename.Spat
#' @export
rename_with.SpatVector <- function(.data, .fn, .cols = everything(), ...) {
  # Use template
  df <- as_tibble(.data[1])
  .fn <- rlang::as_function(.fn)
  .cols <- rlang::enquo(.cols)

  df_rename <- dplyr::rename_with(
    df,
    .fn,
    !!.cols,
    ...
  )

  final_vect <- .data
  names(final_vect) <- names(df_rename)

  return(final_vect)
}

#' @export
dplyr::rename

#' @export
dplyr::rename_with
