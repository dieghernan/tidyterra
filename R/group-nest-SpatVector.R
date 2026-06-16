#' Nest grouped `SpatVector` rows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `group_nest()` and `nest_by()` create tibbles with list-columns containing
#' `SpatVector` objects.
#'
#' @export
#' @encoding UTF-8
#' @rdname group_nest.SpatVector
#' @name group_nest.SpatVector
#'
#' @keywords internal
#' @seealso
#' [dplyr::group_nest()], [dplyr::nest_by()], [nest.SpatVector()],
#' [terra::svc()]
#'
#' @family dplyr.group_functions
#'
#' @importFrom dplyr group_nest
#'
#' @inheritParams dplyr::group_nest
#'
#' @param .tbl,.data A `SpatVector`.
#' @returns A tibble with a list-column of `SpatVector` objects.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::svc()].
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::group_nest()] family for
#' `SpatVector` objects.
#'
#' ## `SpatVector`
#'
#' The nested list-column contains `SpatVector` objects, preserving the
#' geometries for each group.
#'
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v$grp <- rep(c("A", "B"), length.out = nrow(v))
#'
#' group_nest(v, grp)
#'
#' nest_by(v, grp)
#'
#' # Convert to a named SpatVectorCollection.
#' nested <- group_nest(v, grp)
#'
#' sv <- pull(nested, data)
#' names(sv) <- pull(nested, grp)
#'
#' terra::svc(sv)
group_nest.SpatVector <- function(.tbl, ..., .key = "data", keep = FALSE) {
  groups <- dplyr::group_keys(group_by(.tbl, ..., .add = TRUE))
  split <- group_split(group_by(.tbl, ..., .add = TRUE), .keep = keep)

  groups[[.key]] <- split
  groups
}

#' @export
dplyr::group_nest

#' @export
#' @encoding UTF-8
#' @rdname group_nest.SpatVector
#' @importFrom dplyr nest_by
nest_by.SpatVector <- function(.data, ..., .key = "data", .keep = FALSE) {
  nested <- group_nest(.data, ..., .key = .key, keep = .keep)
  dplyr::rowwise(nested, dplyr::all_of(setdiff(names(nested), .key)))
}

#' @export
dplyr::nest_by
