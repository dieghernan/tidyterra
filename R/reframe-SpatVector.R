#' Reframe each group of a `SpatVector`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `reframe()` can return any number of rows per group. The geometry of each
#' group is aggregated and repeated for each row created for that group.
#'
#' @rdname reframe.SpatVector
#' @name reframe.SpatVector
#'
#' @seealso [dplyr::reframe()].
#'
#' @family dplyr.groups
#'
#' @importFrom dplyr reframe
#'
#' @inheritParams summarise.SpatVector
#' @inheritParams dplyr::reframe
#'
#' @returns A `SpatVector`.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::reframe()] method for
#' `SpatVector` objects.
#'
#' For grouped inputs and calls using `.by`, geometries are aggregated per
#' group. If a group produces more than one row, the aggregated group geometry
#' is repeated for each output row.
#'
#' @encoding UTF-8
#' @export
#' @examples
#' v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#' v$grp <- rep(c("A", "B"), length.out = nrow(v))
#'
#' v |>
#'   reframe(value = c(min(as.double(cpro)), max(as.double(cpro))), .by = grp)
#'
#' v |>
#'   rowwise() |>
#'   reframe(value = 1:2)
reframe.SpatVector <- function(.data, ..., .by = NULL, .dissolve = TRUE) {
  check_bool(.dissolve)

  tbl <- as_tibble(.data)
  by_groups <- group_by(.data, {{ .by }})

  if (is_rowwise_spatvector(.data)) {
    if (!rlang::quo_is_null(rlang::enquo(.by))) {
      cli::cli_abort(
        "{.arg .by} is not supported for rowwise {.cls SpatVector} inputs."
      )
    }

    ind <- make_safe_index("tterra_index", tbl)
    work <- dplyr::ungroup(tbl)
    work[[ind]] <- seq_len(nrow(work))

    reframed <- dplyr::reframe(work, ..., .by = dplyr::all_of(ind))
    geom_tbl <- as_tibble(.data[, 0], geom = "WKT")
    geom_tbl[[ind]] <- seq_len(nrow(geom_tbl))
    names(geom_tbl)[names(geom_tbl) == "geometry"] <- "tterra_geometry"

    joined <- dplyr::left_join(reframed, geom_tbl, by = ind)
    joined$geometry <- joined$tterra_geometry
    joined$tterra_geometry <- NULL
    joined[[ind]] <- NULL
    joined <- restore_attr(joined, ungroup_spat_template(.data))

    return(as_spat_internal(joined))
  }

  reframed <- dplyr::reframe(tbl, ..., .by = {{ .by }})

  if (is_grouped_spatvector(.data)) {
    group_tbl <- dplyr::group_keys(tbl)
    spatv <- .data[seq_len(nrow(.data)), ]
    spatv$tterra_index <- dplyr::group_indices(tbl)
    geom <- terra::aggregate(spatv, by = "tterra_index", dissolve = .dissolve)
  } else if (is_grouped_spatvector(by_groups)) {
    group_tbl <- dplyr::group_keys(as_tibble(by_groups))
    spatv <- .data[seq_len(nrow(.data)), ]
    spatv$tterra_index <- dplyr::group_indices(as_tibble(by_groups))
    geom <- terra::aggregate(spatv, by = "tterra_index", dissolve = .dissolve)
  } else {
    group_tbl <- tibble::tibble()
    geom <- terra::aggregate(.data, dissolve = .dissolve)
  }

  geom_tbl <- dplyr::bind_cols(group_tbl, as_tibble(geom[, 0], geom = "WKT"))
  names(geom_tbl)[names(geom_tbl) == "geometry"] <- "tterra_geometry"

  if (ncol(group_tbl) > 0) {
    joined <- dplyr::left_join(reframed, geom_tbl, by = names(group_tbl))
  } else {
    geom_rep <- geom_tbl[rep(1, nrow(reframed)), , drop = FALSE]
    joined <- dplyr::bind_cols(reframed, geom_rep["tterra_geometry"])
  }

  joined$geometry <- joined$tterra_geometry
  joined$tterra_geometry <- NULL
  joined <- restore_attr(joined, ungroup_spat_template(.data))

  as_spat_internal(joined)
}

#' @export
dplyr::reframe

ungroup_spat_template <- function(x) {
  tbl <- as_tbl_internal(x)
  class(tbl) <- setdiff(class(tbl), c("grouped_df", "rowwise_df"))
  attr(tbl, "groups") <- NULL
  tbl
}
