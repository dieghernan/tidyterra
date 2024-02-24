#' Extract a single layer/attribute
#'
#' @description
#' `pull()` is similar to $ on a data frame. It's mostly useful because it
#' looks a little nicer in pipes and it can optionally name the output.
#'
#' **It is possible to extract the geographic coordinates of a SpatRaster**.
#' You need to use `pull(.data, x, xy = TRUE)`. `x` and `y` are reserved
#' names on terra, since they refer to the geographic coordinates of the layer.
#'
#' See **Examples** and section **About layer names** on [as_tibble.Spat()].
#'
#' @export
#' @rdname pull.Spat
#' @name pull.Spat
#'
#' @seealso [dplyr::pull()]
#'
#' @family dplyr.cols
#' @family dplyr.methods
#'
#' @importFrom dplyr pull
#'
#' @inheritParams select.Spat
#' @param var A variable specified as:
#'
#'   - a literal layer/attribute name
#'   - a positive integer, giving the position counting from the left
#'   - a negative integer, giving the position counting from the right.
#'
#' The default returns the last layer/attribute (on the assumption that's the
#' column you've created most recently).
#'
#' @param name An optional parameter that specifies the column to be used as
#'   names for a named vector. Specified in a similar manner as `var`.
#' @param ...  Arguments passed on to [as_tibble()]
#'
#' @return A vector the same number of cells/geometries as `.data`.
#'
#' On SpatRasters, note that the default (`na.rm = FALSE`) would remove
#' empty cells, so you may need to pass (`na.rm = FALSE`) to `...`. See
#' [terra::as.data.frame()].
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::values()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::pull()] function. This is done
#' by coercing the Spat* object to a tibble first (see [as_tibble.Spat]) and
#' then using [dplyr::pull()] method over the tibble.
#'
#' ## SpatRaster
#'
#' When passing option `na.rm = TRUE` to `...`, only cells with a value
#' distinct to `NA` are extracted. See [terra::as.data.frame()].
#'
#' If `xy = TRUE` option is passed to `...`, two columns names `x` and `y`
#' (corresponding to the geographic coordinates of each cell) are available
#' in position `1` and `2`. Hence, `pull(.data, 1)` and
#' `pull(.data, 1, xy = TRUE)` return different result.
#'
#' ## SpatVector
#'
#' When passing `geom = "WKT"/geom = "HEX"` to `...`,  the geometry of the
#' SpatVector can be pulled passing `var = geometry`. Similarly to SpatRaster
#' method, when using `geom = "XY"` the `x,y` coordinates can be pulled with
#' `var = x/var = y`. See  [terra::as.data.frame()] options.
#'
#' @examples
#'
#' library(terra)
#' f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
#' r <- rast(f)
#'
#' # Extract second layer
#' r %>%
#'   pull(2) %>%
#'   head()
#'
#' # With xy the first two cols are `x` (longitude) and `y` (latitude)
#'
#' r %>%
#'   pull(2, xy = TRUE) %>%
#'   head()
#'
#' # With renaming
#'
#' r %>%
#'   mutate(cat = cut(cyl_tile_3, c(0, 100, 300))) %>%
#'   pull(cyl_tile_3, name = cat) %>%
#'   head()
#'
pull.SpatRaster <- function(.data, var = -1, name = NULL, ...) {
  var <- rlang::enquo(var)
  name <- rlang::enquo(name)
  return(dplyr::pull(as_tibble(.data, ...), !!var, !!name))
}

#' @export
#' @rdname pull.Spat
pull.SpatVector <- function(.data, var = -1, name = NULL, ...) {
  var <- rlang::enquo(var)
  name <- rlang::enquo(name)
  return(dplyr::pull(as_tibble(.data, ...), !!var, !!name))
}

#' @export
dplyr::pull
