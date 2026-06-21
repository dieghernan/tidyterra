#' Extract a single layer/attribute
#'
#' @description
#' `pull()` is similar to `$` on a data frame. It is mostly useful because it
#' looks nicer in pipes and can optionally name the output.
#'
#' **You can extract the geographic coordinates of a `SpatRaster`**.
#' Use `pull(.data, x, xy = TRUE)`. `x` and `y` are reserved
#' names on terra, since they refer to the geographic coordinates of the layer.
#'
#' See **Examples** and section **About layer names** on [as_tibble.Spat()].
#'
#' @rdname pull.Spat
#' @name pull.Spat
#'
#' @seealso [dplyr::pull()].
#'
#' @family dplyr.cols
#'
#' @importFrom dplyr pull
#'
#' @inheritParams select.Spat
#' @inheritParams dplyr::pull
#'
#' @param var A variable specified as:
#'   - a literal layer/attribute name.
#'   - a positive integer, giving the position counting from the left.
#'   - a negative integer, giving the position counting from the right.
#'
#'   The default returns the last layer/attribute (on the assumption that's the
#'   column you've created most recently).
#'
#'   This argument is taken by expression and supports
#'   [quasiquotation][rlang::topic-inject] (you can unquote column names and
#'   column locations).
#'
#' @param ... Arguments passed to [as_tibble.SpatRaster()] or
#'   [as_tibble.SpatVector()] methods.
#'
#' @returns A vector with the same number of cells/geometries as `.data`.
#'
#' On `SpatRaster` objects, note that the default (`na.rm = FALSE`) removes
#' empty cells, so you may need to pass (`na.rm = FALSE`) to `...`. See
#' [terra::as.data.frame()].
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::values()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::pull()] methods. Each method first
#' coerces the `Spat*` object to a tibble (see [as_tibble.Spat]) and then
#' applies [dplyr::pull()] to the tibble.
#'
#' ## `SpatRaster`
#'
#' When passing `na.rm = TRUE` to `...`, only cells with a value other than
#' `NA` are extracted. See [terra::as.data.frame()].
#'
#' If `xy = TRUE` is passed to `...`, two columns named `x` and `y`
#' (corresponding to the geographic coordinates of each cell) are available
#' in positions `1` and `2`. Therefore, `pull(.data, 1)` and
#' `pull(.data, 1, xy = TRUE)` return different results.
#'
#' ## `SpatVector`
#'
#' When passing `geom = "WKT"` or `geom = "HEX"` to `...`, the geometry of the
#' `SpatVector` can be extracted with `var = geometry`. Similarly, when using
#' `geom = "XY"`, the coordinates can be extracted with `var = x` or
#' `var = y`. See the options in [terra::as.data.frame()].
#'
#' @encoding UTF-8
#' @export
#' @examples
#'
#' library(terra)
#' f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
#' r <- rast(f)
#'
#' # Extract the second layer.
#' r |>
#'   pull(2) |>
#'   head()
#'
#' # With `xy`, the first two columns are `x` (longitude) and `y` (latitude).
#'
#' r |>
#'   pull(2, xy = TRUE) |>
#'   head()
#'
#' # With renaming
#'
#' r |>
#'   mutate(cat = cut(cyl_tile_3, c(0, 100, 300))) |>
#'   pull(cyl_tile_3, name = cat) |>
#'   head()
#'
pull.SpatRaster <- function(.data, var = -1, name = NULL, ...) {
  var <- rlang::enquo(var)
  name <- rlang::enquo(name)
  dplyr::pull(as_tibble(.data, ...), !!var, !!name)
}

#' @rdname pull.Spat
#' @export
pull.SpatVector <- function(.data, var = -1, name = NULL, ...) {
  var <- rlang::enquo(var)
  name <- rlang::enquo(name)
  dplyr::pull(as_tibble(.data, ...), !!var, !!name)
}

#' @export
dplyr::pull
