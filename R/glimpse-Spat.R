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
  dplyr::glimpse(as_tibble(x, ...), width = width)
  return(invisible(x))
}

#' @rdname glimpse.Spat
#' @export
glimpse.SpatVector <- function(x, width = NULL, ...) {
  # Geometry type
  cli::cat_line("Geometry type: ", tools::toTitleCase(terra::geomtype(x)))

  # CRS
  crsnamed <- get_named_crs(x)

  if (!is.na(crsnamed)) {
    pulled_crs <- pull_crs(x)
    if (sf::st_is_longlat(pulled_crs)) {
      cli::cat_line("Geodetic CRS: ", crsnamed)
    } else {
      cli::cat_line("Projected CRS: ", crsnamed)
      unts <- try(sf::st_crs(pulled_crs)$units, silent = TRUE)
      if (!inherits(unts, "try-error") && !is.null(unts) && !is.na(unts)) {
        cli::cat_line("CRS projection units: ", unts)
      }
    }
  }

  # Extent
  ext <- as.vector(terra::ext(x))
  is_lonlat <- sf::st_is_longlat(pull_crs(x))

  if (is_lonlat) {
    lons <- lapply(ext[c("xmin", "xmax")], decimal_to_degrees, type = "lon")

    lats <- lapply(ext[c("ymin", "ymax")], decimal_to_degrees, type = "lat")

    ext_fmt <- unlist(c(lons, lats))
  } else {
    big_mark <- ","

    if (identical(getOption("OutDec"), ",")) big_mark <- "."
    ext_fmt <- format(ext, big.mark = big_mark, justify = "right")
  }

  xfmt <- paste(ext_fmt[c("xmin", "xmax")],
    collapse = " - "
  )
  yfmt <- paste(ext_fmt[c("ymin", "ymax")],
    collapse = " - "
  )

  extnamed <- paste0("[", xfmt, "] , [", yfmt, "]")


  cli::cat_line("Extent (x , y) : ", extnamed)

  # Regular data frame (with options if provided)
  dplyr::glimpse(as_tibble(x, ...), width = width)
  return(invisible(x))
}


#' @export
dplyr::glimpse



get_named_crs <- function(x) {
  # Based in terra:::.name_or_proj4()
  pulled <- pull_crs(x)

  d <- terra::crs(pulled, describe = TRUE)
  r <- terra::crs(pulled, proj = TRUE)

  if (!(d$name %in% c(NA, "unknown", "unnamed"))) {
    if (substr(r, 1, 13) == "+proj=longlat") {
      r <- paste("lon/lat", d$name)
    } else {
      r <- d$name
    }
    if (!is.na(d$code)) {
      r <- paste0(r, " (", d$authority, ":", d$code, ")")
    }
  }

  if (r == "") {
    try <- unlist(strsplit(pulled, "\n"))[1]
    try <- unlist(strsplit(try, "[", fixed = TRUE))[[2]]
    try <- gsub('"|,$', "", try)
    r <- try
  }

  if (is.na(r) || r == "" || is.null(r)) r <- NA

  return(r)
}


# To convert lon lat from decimal to pretty
decimal_to_degrees <- function(x, type) {
  coordinit <- x
  x <- abs(x)
  D <- as.integer(x)
  m <- (x - D) * 60
  M <- as.integer(m)
  S <- round((m - M) * 60, 4)

  if (type == "lon") {
    if (coordinit > 0) {
      lab <- "E"
    } else {
      lab <- "W"
    }
  } else {
    if (coordinit > 0) {
      lab <- "N"
    } else {
      lab <- "S"
    }
  }

  label <- paste0(D, "\u00b0 ", M, "' ", S, '\" ', lab)
  return(label)
}
