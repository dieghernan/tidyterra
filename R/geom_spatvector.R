#' Visualise SpatVector objects
#'
#' @description
#'
#' Wrappers of [ggplot2::geom_sf()] family used to visualise SpatVector objects
#' (see [terra::vect()]).
#'
#'
#' @return A ggplot2 layer
#' @family ggplot2.utils
#' @param data A SpatRaster object.
#'
#' @name ggspatvector
#'
#' @param data A SpatVector object, see [terra::vect()].
#'
#' @param ... Other arguments passed on to [ggplot2::geom_sf()] functions.
#'   These are often aesthetics, used to set an aesthetic to a fixed value,
#'   like `colour = "red"` or `linewidth = 3`.
#'
#' @inheritParams ggplot2::geom_sf
#'
#' @seealso [ggplot2::geom_sf()]
#'
#' @details
#'
#' These functions are wrappers of [ggplot2::geom_sf()] functions. Since a
#' [fortify.SpatVector()] method is provided, **ggplot2** treat a SpatVector
#' in the same way that a `sf` object. A side effect is that you can use
#' [ggplot2::geom_sf()] directly with SpatVectors.
#'
#' See [ggplot2::geom_sf()] for details on aesthetics, etc.
#'
#' @section  terra equivalent:
#'
#' [terra::plot()]
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a SpatVector
#' extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#'
#' cyl <- terra::vect(extfile)
#' class(cyl)
#'
#' library(ggplot2)
#'
#' ggplot(cyl) +
#'   geom_spatvector()
#'
#'
#' # With params
#'
#' ggplot(cyl) +
#'   geom_spatvector(aes(fill = name), color = NA) +
#'   scale_fill_viridis_d() +
#'   coord_sf(crs = 3857)
#'
#' # Add labels
#' ggplot(cyl) +
#'   geom_spatvector(aes(fill = name), color = NA) +
#'   geom_spatvector_text(aes(label = iso2),
#'     fontface = "bold",
#'     color = "red"
#'   ) +
#'   scale_fill_viridis_d(alpha = 0.4) +
#'   coord_sf(crs = 3857)
#'
#' # You can use now geom_sf with SpatVectors!
#'
#'
#'
#' ggplot(cyl) +
#'   geom_sf() +
#'   labs(
#'     title = paste("cyl is", as.character(class(cyl))),
#'     subtitle = "With geom_sf()"
#'   )
#' }
geom_spatvector <- function(mapping = aes(),
                            data = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            ...) {
  ggplot2::geom_sf(
    data = data,
    mapping = mapping,
    na.rm = na.rm,
    show.legend = show.legend,
    ...
  )
}

#' @export
#' @name ggspatvector
geom_spatvector_label <- function(mapping = aes(),
                                  data = NULL,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  ...,
                                  nudge_x = 0,
                                  nudge_y = 0,
                                  label.size = 0.25,
                                  inherit.aes = TRUE) {
  ggplot2::geom_sf_label(
    mapping = mapping,
    data = data,
    ...,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    label.size = label.size,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}


#' @export
#' @name ggspatvector
geom_spatvector_text <- function(mapping = aes(),
                                 data = NULL,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 ...,
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 check_overlap = FALSE,
                                 inherit.aes = TRUE) {
  ggplot2::geom_sf_text(
    mapping = mapping,
    data = data,
    ...,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    check_overlap = check_overlap,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @export
#' @name ggspatvector
stat_spatvector <- function(mapping = NULL,
                            data = NULL,
                            geom = "rect",
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
  # nocov start
  ggplot2::stat_sf(
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    ...
  )
  # nocov end
}
