#' Get a glimpse of your Spat* objects
#'
#' @description
#' `glimpse()` is like a transposed version of [print()]: layers/columns run
#' down the  page, and data runs across. This makes it possible to see every
#' layer/column in a Spat* object.
#'
#' @export
#' @rdname glimpse.Spat
#' @name glimpse.Spat
#'
#' @seealso [dplyr::glimpse()]
#'
#' @family dplyr.cols
#' @family dplyr.methods
#'
#' @importFrom dplyr glimpse
#'
#'
#' @return original `x` is (invisibly) returned, allowing `glimpse()` to
#' be used within a data pipeline.
#'
#' @inheritParams as_tibble.Spat
#' @param ... Arguments passed on to [`as_tibble()`][as_tibble.Spat] Spat
#'   methods.
#' @param width  Width of output: defaults to the setting of the
#'   `width` if finite (see [dplyr::glimpse()]) or the width of the console.
#' @section terra equivalent:
#'
#' `print()`
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::glimpse()] function for
#' Spat*. objects.
#'
#'
#' @examples
#'
#' library(terra)
#'
#' # SpatVector
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' v %>% glimpse(width = 200)
#'
#' # Use on a pipeline
#' v %>%
#'   glimpse() %>%
#'   mutate(a = 30) %>%
#'   # with options
#'   glimpse(geom = "WKT")
#'
#' # SpatVector
#' r <- rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))
#'
#' r %>% glimpse()
#'
#' # Use on a pipeline
#' r %>%
#'   glimpse() %>%
#'   mutate(b = elevation_m / 100) %>%
#'   # With options
#'   glimpse(xy = TRUE)
glimpse.SpatRaster <- function(x, width = NULL, ...) {
  init <- x
  # Use sf method
  dplyr::glimpse(as_tibble(init, ...), width = width)

  return(invisible(x))
}

#' @rdname glimpse.Spat
#' @export
glimpse.SpatVector <- function(x, width = NULL, ...) {
  init <- x
  dplyr::glimpse(as_tibble(init, ...), width = width)

  return(invisible(x))
}


#' @export
dplyr::glimpse
