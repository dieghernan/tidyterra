#' Compare attributes of two SpatRasters
#'
#' @description
#'
#' Two SpatRasters are compatible (in terms of combining layers) if the
#' crs, extent and resolution are similar. In those cases you can combine
#' the SpatRasters simply as `c(x, y)`.
#'
#' This function compares those attributes informing of the results. See
#' **Solving issues** section for minimal guidance.
#'
#' @return
#' A invisible logical `TRUE/FALSE` indicating if the SpatRasters are
#' compatible, plus an informative message flagging the issues found (if any).
#'
#'
#' @param x,y SpatRaster objects
#' @param digits Integer to set the precision for comparing the extent and
#'   the resolution.
#'
#' @family helpers
#' @export
#'
#' @section Solving issues:
#'
#' On **non-equal crs**, try [terra::project()]. On **non-equal extent** try
#' [terra::resample()]. On **non-equal resolution** you can try
#' [terra::resample()], [terra::aggregate()] or [terra::disagg()].
#'
#' @examples
#' library(terra)
#'
#' x <- rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")
#'
#' # Nothing
#' compare_spatrasters(x, x)
#'
#' # Different crs
#' y_nocrs <- x
#' crs(y_nocrs) <- NA
#'
#' compare_spatrasters(x, y_nocrs)
#'
#' # Different extent
#' compare_spatrasters(x, x[1:10, , drop = FALSE])
#'
#' # Different resolution
#' y_newres <- x
#'
#' res(y_newres) <- res(x) / 2
#' compare_spatrasters(x, y_newres)
#'
#' # Everything
#'
#' compare_spatrasters(x, project(x, "epsg:3035"))
compare_spatrasters <- function(x, y, digits = 6) {
  if (!all(inherits(x, "SpatRaster"), inherits(y, "SpatRaster"))) {
    x_class <- as.character(class(x))
    y_class <- as.character(class(y))


    cli::cli_abort(paste(
      cli::style_bold("x"), "and",
      cli::style_bold("y"),
      "must be SpatRasters.",
      cli::style_bold("x"), "is",
      cli::col_blue(paste0("<", x_class, ">")), ",",
      cli::style_bold("y"), "is",
      cli::col_blue(paste0("<", y_class, ">."))
    ))
  }

  # Check crs
  equal_crs <- terra::crs(x) == terra::crs(y)
  ext1 <- as.vector(terra::ext(x))
  ext2 <- as.vector(terra::ext(y))

  dif_ext <- round(ext1 - ext2, digits = digits)

  equal_ext <- all(dif_ext == 0)

  # Check resolution
  dif_res <- round(terra::res(x) - terra::res(y),
    digits = digits
  )
  equal_res <- all(dif_res == 0)

  # Results
  if (!all(equal_crs, equal_ext, equal_res)) {
    title <- cli::style_bold("Results of compare_spatrasters() (tidyterra)")

    title <- paste(
      title,
      "\nThe following attributes are not equal:\n"
    )
    if (!equal_crs) {
      title <- paste(
        title, "  -",
        cli::col_red("crs"), "\n"
      )
    }
    if (!equal_ext) {
      title <- paste(
        title, "  -",
        cli::col_red("extent"), "\n"
      )
    }
    if (!equal_res) {
      title <- paste(
        title, "  -",
        cli::col_red("resolution"), "\n"
      )
    }


    cli::cli_alert_warning(cli::col_black(title))

    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}
