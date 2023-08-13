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
#' # SpatRaster
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
  # Dimensions
  cli::cat_line("Rows: ", format(terra::nrow(x),
    big.mark = ",",
    decimal.mark = "."
  ))
  cli::cat_line("Columns: ", format(terra::ncol(x),
    big.mark = ",",
    decimal.mark = "."
  ))
  cli::cat_line("Layers: ", format(terra::nlyr(x),
    big.mark = ",",
    decimal.mark = "."
  ))
  cli::cat_line("Cells: ", format(terra::ncell(x),
    big.mark = ",",
    decimal.mark = "."
  ))

  if (isTRUE(sf::st_is_longlat(pull_crs(x)))) {
    rs <- lapply(terra::res(x), decimal_to_degrees, type = "null")
    rs <- paste0(unlist(rs), collapse = " , ")
  } else {
    rs <- paste(
      format(terra::res(x),
        big.mark = ",",
        decimal.mark = "."
      ),
      collapse = " , "
    )
  }
  cli::cat_line("Resolution (x , y): ", rs)

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
  } else {
    cli::cat_line("CRS: Not Defined / Empty")
  }

  # Extent
  ext <- as.vector(terra::ext(x))
  is_lonlat <- sf::st_is_longlat(pull_crs(x))

  if (isTRUE(is_lonlat)) {
    lons <- lapply(ext[c("xmin", "xmax")], decimal_to_degrees, type = "lon")

    lats <- lapply(ext[c("ymin", "ymax")], decimal_to_degrees, type = "lat")

    ext_fmt <- unlist(c(lons, lats))
  } else {
    ext_fmt <- format(ext,
      big.mark = ",",
      decimal.mark = ".", justify = "right"
    )
  }

  xfmt <- paste(ext_fmt[c("xmin", "xmax")],
    collapse = " - "
  )
  yfmt <- paste(ext_fmt[c("ymin", "ymax")],
    collapse = " - "
  )

  extnamed <- paste0("[", xfmt, "] , [", yfmt, "]")
  cli::cat_line("Extent (x , y) : ", extnamed)

  # Check RGB
  rgb_info <- terra::RGB(x)
  if (!is.null(rgb_info) && length(rgb_info) >= 1) {
    title <- paste0("Raster with ", length(rgb_info), " RGB channels: ")
    ch_name <- names(x)[rgb_info]
    names(ch_name) <- c(
      "Red", "Green",
      "Blue", "Alpha"
    )[seq_len(length(rgb_info))]
    ch_end <- paste0(ch_name, " (", names(ch_name), ")", collapse = ", ")

    cli::cat_line(title, ch_end)
  }

  # Check coltab
  coltab_info <- terra::has.colors(x)
  if (any(coltab_info)) {
    lcol <- length(coltab_info[coltab_info == TRUE])
    title <- paste0("Raster with ", lcol, " color table in: ")
    if (lcol > 1) title <- gsub("table", "tables", title)


    ch_name <- names(x)[coltab_info == TRUE]
    ch_end <- paste0(ch_name, collapse = ", ")

    cli::cat_line(title, ch_end)
  }


  # Regular data frame (with options if provided)
  capt <- utils::capture.output(dplyr::glimpse(as_tibble(x, ...),
    width = width
  ))

  l <- ifelse(terra::nlyr(x) > 1, "Layers:", "Layer:")

  cli::cat_line(l)
  cli::cat_line(capt[-c(1:2)])

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
  } else {
    cli::cat_line("CRS: Not Defined / Empty")
  }

  # Extent
  ext <- as.vector(terra::ext(x))
  is_lonlat <- sf::st_is_longlat(pull_crs(x))

  if (isTRUE(is_lonlat)) {
    lons <- lapply(ext[c("xmin", "xmax")], decimal_to_degrees, type = "lon")

    lats <- lapply(ext[c("ymin", "ymax")], decimal_to_degrees, type = "lat")

    ext_fmt <- unlist(c(lons, lats))
  } else {
    ext_fmt <- format(ext,
      big.mark = ",",
      decimal.mark = ".", justify = "right"
    )
  }

  xfmt <- paste(ext_fmt[c("xmin", "xmax")],
    collapse = " - "
  )
  yfmt <- paste(ext_fmt[c("ymin", "ymax")],
    collapse = " - "
  )

  extnamed <- paste0("[", xfmt, "] , [", yfmt, "]")


  cli::cat_line("Extent (x , y) : ", extnamed)
  if (ncol(x) == 0) {
    cli::cat_line("SpatVector with no attributes (only geometries)")
  } else {
    # Regular data frame (with options if provided)
    dplyr::glimpse(as_tibble(x, ...), width = width)
  }
  return(invisible(x))
}


#' @export
dplyr::glimpse



get_named_crs <- function(x) {
  # Based in terra:::.name_or_proj4()
  pulled <- pull_crs(x)

  d <- try(terra::crs(pulled, describe = TRUE), silent = TRUE)

  if (inherits(d, "try-error")) {
    return(NA)
  }

  r <- terra::crs(pulled, proj = TRUE)

  # nocov start
  if (inherits(r, "try-error")) {
    return(NA)
  }
  # nocov end

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

  # nocov start
  if (is.na(r) || r == "" || is.null(r)) r <- NA
  # nocov end
  return(r)
}


# To convert lon lat from decimal to pretty
decimal_to_degrees <- function(x, type = c("lon", "lat", "null")) {
  type <- match.arg(type)
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
  } else if (type == "lat") {
    if (coordinit > 0) {
      lab <- "N"
    } else {
      lab <- "S"
    }
  } else {
    lab <- NULL
  }

  if (type %in% c("lon", "lat")) {
    label <- paste0(D, "\u00b0 ", M, "' ", S, '\" ', lab)
  } else {
    label <- paste0(
      c(D, M, S),
      c("\u00b0", "'", '\"')
    )
    label <- label[c(D, M, S) != 0]
    label <- paste0(label, collapse = " ")
  }

  return(label)
}
