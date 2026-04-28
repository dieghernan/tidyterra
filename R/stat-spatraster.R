#' @export
#' @encoding UTF-8
#' @rdname geom_spatraster
#'
#' @description
#'
#' `stat_spatraster()` complements [geom_spatraster()] when you need to change
#' the `geom`.
#'
#' @param geom Geom used to display the data. Recommended values for
#'   `SpatRaster` are `"raster"` (the default), `"point"`, `"text"` and
#'   `"label"`.
#' @seealso
#'
#' Recommended `geoms`:
#' * [ggplot2::geom_point()].
#' * [ggplot2::geom_label()].
#' * [ggplot2::geom_text()].
#'
#' @section Aesthetics:
#'
#' ## `stat_spatraster()`
#'
#' `stat_spatraster()` understands the same aesthetics as `geom_spatraster()`
#' when `geom = "raster"` (the default):
#'
#' * [`fill`][ggplot2::aes_colour_fill_alpha]
#' * [`alpha`][ggplot2::aes_colour_fill_alpha]
#'
#' When `geom = "raster"`, the `fill` argument behaves as in
#' `geom_spatraster()`. If another `geom` is used, `stat_spatraster()`
#' understands the aesthetics required by that `geom`, so
#' `aes(fill = <name_of_lyr>)` is not applicable.
#'
#' The `x` and `y` aesthetics are mapped by default, so you do not need to add
#' them in `aes()`. In every case, aesthetics should be mapped with computed
#' variables. See **Computed variables** and **Examples**.
#' @examples
#' \donttest{
#' # Using stat_spatraster
#' # Default
#' ggplot() +
#'   stat_spatraster(data = temp_rast) +
#'   facet_wrap(~lyr)
#'
#' # Using points
#' ggplot() +
#'   stat_spatraster(
#'     data = temp_rast,
#'     aes(color = after_stat(value)),
#'     geom = "point", maxcell = 250
#'   ) +
#'   scale_colour_viridis_c(na.value = "transparent") +
#'   facet_wrap(~lyr)
#'
#' # Using points and labels
#'
#' r_single <- temp_rast |> select(1)
#'
#' ggplot() +
#'   stat_spatraster(
#'     data = r_single,
#'     aes(color = after_stat(value)),
#'     geom = "point",
#'     maxcell = 2000
#'   ) +
#'   stat_spatraster(
#'     data = r_single,
#'     aes(label = after_stat(round(value, 2))),
#'     geom = "label",
#'     alpha = 0.85,
#'     maxcell = 20
#'   ) +
#'   scale_colour_viridis_c(na.value = "transparent")
#' }
stat_spatraster <- function(
  mapping = aes(),
  data,
  geom = "raster",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = FALSE,
  maxcell = 500000,
  ...
) {
  if (!inherits(data, "SpatRaster")) {
    cli::cli_abort(paste(
      "{.fun tidyterra::stat_spatraster} only works with",
      "{.cls SpatRaster} objects, not {.cls {class(data)}}.",
      "See {.help terra::vect}"
    ))
  }

  # 1. Work with aes ----

  # Prepare aesthetics for `StatTerraSpatRaster`.
  mapping <- cleanup_aesthetics(mapping, "group")

  spatraster <- NULL
  lyr <- NULL

  mapping <- override_aesthetics(
    mapping,
    ggplot2::aes(
      spatraster = spatraster,
      # For faceting
      lyr = lyr,
      group = lyr
    )
  )

  # Only do this when `geom = "raster"` to mimic `geom_spatraster()`.

  if (geom == "raster") {
    dots <- list(...)
    raster_names <- names(data)

    prepared <- prepare_aes_spatraster(mapping, raster_names, dots)

    # Use prepared data
    mapping <- prepared$map

    # Check if need to subset the SpatRaster
    if (is.character(prepared$namelayer)) {
      # Subset the layer from the data
      data <- terra::subset(data, prepared$namelayer)
    }
  }
  # 2. Check if resample is needed----

  # Check mixed types
  data <- check_mixed_cols(data)

  data <- resample_spat(data, maxcell)

  # 3. Create a nested list with each layer----
  raster_list <- as.list(data)

  # Now create the data frame
  data_tbl <- tibble::tibble(
    spatraster = list(NULL),
    # For faceting: As factors for keeping orders
    lyr = factor(names(data), levels = names(data))
  )

  names(data_tbl$spatraster) <- names(data)

  # Each layer to a row
  for (i in seq_len(terra::nlyr(data))) {
    data_tbl$spatraster[[i]] <- raster_list[[i]]
  }

  # 4. Build layer ----

  crs_terra <- pull_crs(data)

  # Create layer
  layer_spatrast <- ggplot2::layer(
    data = data_tbl,
    mapping = mapping,
    stat = StatTerraSpatRaster,
    geom = geom,
    position = "identity",
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(
      na.rm = na.rm,
      # Extra params
      maxcell = maxcell,
      ...
    )
  )

  # From `ggspatial`.
  # If the `SpatRaster` has a CRS, add an empty `geom_sf()` to train the
  # scales. This mimics using the first layer CRS as the base CRS for
  # `coord_sf()`.

  if (!is.na(crs_terra)) {
    layer_spatrast <- c(
      layer_spatrast,
      ggplot2::geom_sf(
        data = sf::st_sfc(sf::st_point(), crs = crs_terra),
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    )
  }

  layer_spatrast
}
