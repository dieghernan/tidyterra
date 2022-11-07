#' @export
#' @rdname geom_spatraster
#'
#' @description
#'
#' `stat_spatraster()` is provided as a complementary function, so the `geom`
#' can be modified.
#'
#' @param geom The geometric object to use display the data. Recommended `geom`
#'   for SpatRaster are `"raster"` (the default), `"point"`,`"text"` and
#'   `"label"`.
#' @seealso Alternative geoms: [ggplot2::geom_point()], [ggplot2::geom_label()],
#'   [ggplot2::geom_text()].
#'
#'
#' @section Aesthetics:
#'
#'  ## stat_spatraster()
#'
#' `stat_spatraster()` understands the same aesthetics than `geom_spatraster()`
#' when using `geom = "raster"` (the default):
#'
#' - `fill`
#' - `alpha`
#'
#' When `geom = "raster"` the `fill` parameter would behave as in
#' `geom_spatraster()`. If another `geom` is used `stat_spatraster()` would
#' understand the aesthetics of the required `geom` and
#' `aes(fill = <name_of_lyr>)` would not be applicable.
#'
#' Note also that mapping of aesthetics `x` and `y` is provided by default,
#' so the user does not need to add those aesthetics on `aes()`. In all the
#' cases the aesthetics should be mapped by using computed variables. See
#' section **Computed variables** and **Examples**.
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
#'   scale_colour_viridis_c(na.value = NA) +
#'   facet_wrap(~lyr)
#'
#' # Using points and labels
#'
#' r_single <- temp_rast %>% select(1)
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
#'   scale_colour_viridis_c(na.value = NA)
#' }
stat_spatraster <- function(mapping = aes(),
                            data,
                            geom = "raster",
                            na.rm = TRUE,
                            show.legend = NA,
                            inherit.aes = FALSE,
                            maxcell = 500000,
                            ...) {
  if (!inherits(data, "SpatRaster")) {
    stop(
      "geom_spatraster() only works with SpatRaster objects. ",
      "See ?terra::vect"
    )
  }


  # 1. Work with aes ----

  # Prepare aes for StatTerraSpatRaster
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

  # Do this only if provided geom is raster
  # to mimick geom_spatraster

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


  # From ggspatial
  # If the SpatRaster has crs add a geom_sf for training scales
  # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
  # using the first layer's CRS as the base CRS for coord_sf().

  if (!is.na(crs_terra)) {
    layer_spatrast <- c(
      layer_spatrast,
      ggplot2::geom_sf(
        data = sf::st_sfc(sf::st_point(),
          crs = crs_terra
        ),
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    )
  }


  layer_spatrast
}
