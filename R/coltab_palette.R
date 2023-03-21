#' Extract a color palette from a color table associated with a SpatRaster
#'
#' @description
#'
#' Some categorical SpatRaster may have an associated color table. This
#' function extract those values.
#'
#' @return
#' A named vector of hex colors or `NULL` if no color table is found
#'
#'
#' @param x SpatRaster objects
#'
#' @family helpers
#' @export
#'
#' @section  terra equivalent:
#'
#' [terra::coltab()]
#'
#' @seealso [terra::coltab()]
#'
#' @examples
#' library(terra)
#' # Corine Land Cover v 2018
#' r <- rast(system.file("extdata/clc_2018_majorca.tif", package = "tidyterra"))
#'
#' plot(r)
#' # Get coltab
#' coltab_pal <- coltab_palette(r)
#'
#' scales::show_col(coltab_pal, labels = FALSE)
#'
#' \donttest{
#' # With ggplot2 + tidyterra
#' library(ggplot2)
#'
#' gg <- ggplot() +
#'   # Does not show legend as it is huge on this example
#'   geom_spatraster(data = r, show.legend = FALSE)
#'
#'
#' # Default plot
#' gg
#'
#' # With coltabs
#' gg +
#'   scale_fill_manual(values = coltab_pal, na.value = NA)
#' }
coltab_palette <- function(x) {
  if (!inherits(x, "SpatRaster")) {
    cli::cli_alert_info(
      paste(
        cli::col_blue("`x`"), "is not a SpatVector\nReturning",
        cli::col_blue("`NULL`")
      )
    )
    return(NULL)
  }

  if (!any(terra::has.colors(x))) {
    cli::cli_alert_info(
      paste(
        cli::col_blue("`x`"), "does not have a color table\nReturning",
        cli::col_blue("`NULL`")
      )
    )
    return(NULL)
  }

  lcats <- terra::cats(x)
  names(lcats) <- names(x)
  withnames <- dplyr::bind_rows(lcats)

  ctab <- make_safe_index("coltab", withnames)
  cats <- dplyr::bind_rows(lcats, .id = ctab)

  # Keep those that are not colors
  cats_no_col <- cats[, !grepl("red|green|blue", names(cats),
    ignore.case = TRUE
  )]

  names(cats_no_col) <- gsub("values", "value", tolower(names(cats_no_col)))

  # Get cols with alpha
  cols_alpha_l <- terra::coltab(x)
  names(cols_alpha_l) <- names(x)
  cols_alpha <- dplyr::bind_rows(cols_alpha_l, .id = ctab)

  names(cols_alpha) <- gsub("values", "value", tolower(names(cols_alpha)))

  # Join and create
  tojoin <- intersect(names(cats_no_col), names(cols_alpha))

  endcols <- dplyr::left_join(cats_no_col, cols_alpha, by = tojoin)

  # Create palette
  namedpal <- rgb(endcols[, grepl("red|green|blue|alpha", names(endcols))],
    maxColorValue = 255
  )

  # Identify col with labels
  getnames <- unlist(lapply(endcols, is.character))
  colnames <- names(getnames[getnames == TRUE])
  colnames <- colnames[colnames != ctab][1]
  names(namedpal) <- endcols[[colnames]]


  namedpal
}
