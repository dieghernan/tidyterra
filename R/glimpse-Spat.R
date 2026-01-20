#' Get a nice glimpse of your `Spat*` objects
#'
#' @description
#' `glimpse()` is like a transposed version of [print()]: layers/columns run
#' down the  page, and data runs across. This makes it possible to see every
#' layer/column in a `Spat*` object.
#'
#' @export
#' @rdname glimpse.Spat
#' @name glimpse.Spat
#'
#' @seealso [tibble::print.tbl_df()]
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
#' @inheritParams tibble::print.tbl_df
#'
#' @param width Width of output: defaults to the setting of the width option
#'   (if finite) or the width of the console. See [dplyr::glimpse()].
#' @param ... Arguments passed on to [`as_tibble()`][as_tibble.Spat] methods
#'   for `SpatRaster` and `SpatVector`. See [as_tibble.SpatRaster()].
#' @param max_extra_cols Number of extra columns or layers to print abbreviated
#'   information for, if `n` is too small for the `Spat*` object.
#' @param n Maximum number of rows to show.
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' `print()`
#'
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::glimpse()] function for
#' `Spat*`. objects.
#'
#'
#' @examples
#'
#' library(terra)
#'
#' # SpatVector
#' v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
#'
#' v |> glimpse(n = 2)
#'
#' # Use on a pipeline
#' v |>
#'   glimpse() |>
#'   mutate(a = 30) |>
#'   # with options
#'   glimpse(geom = "WKT")
#'
#' # SpatRaster
#' r <- rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))
#'
#' r |> glimpse()
#'
#' # Use on a pipeline
#' r |>
#'   glimpse() |>
#'   mutate(b = elevation_m / 100) |>
#'   # With options
#'   glimpse(xy = TRUE)
glimpse.SpatRaster <- function(
  x,
  width = NULL,
  ...,
  n = 10,
  max_extra_cols = 20
) {
  # Class
  nr <- format(terra::nrow(x), big.mark = ",", decimal.mark = ".")
  nc <- format(terra::ncol(x), big.mark = ",", decimal.mark = ".")
  nl <- format(terra::nlyr(x), big.mark = ",", decimal.mark = ".")
  lay <- ifelse(terra::nlyr(x) == 1, " layer", " layers")
  ncll <- format(terra::ncell(x), big.mark = ",", decimal.mark = ".")

  tterra_header(
    "A SpatRaster ",
    nr,
    " x ",
    nc,
    " x ",
    nl,
    lay,
    " (",
    ncll,
    " cells)"
  )

  # Resolution
  tterra_header_string_res(x)
  # CRS
  tterra_header_string_crs(x)
  # Extent
  tterra_header_string_ext(x)
  # RGB
  tterra_header_string_rgb(x)
  # Coltab
  tterra_header_string_ctab(x)

  # Body
  cli::cat_line() # Empty line

  if (!terra::hasValues(x)) {
    cli::cat_line("SpatRaster with no values")
  } else {
    tterra_body(x, ..., width = width, n = n, max_extra_cols = max_extra_cols)
  }

  invisible(x)
}

#' @rdname glimpse.Spat
#' @export
glimpse.SpatVector <- function(
  x,
  width = NULL,
  ...,
  n = 10,
  max_extra_cols = 20
) {
  # Class
  nr <- format(terra::nrow(x), big.mark = ",", decimal.mark = ".")
  nc <- format(terra::ncol(x), big.mark = ",", decimal.mark = ".")

  tterra_header("A SpatVector ", nr, " x ", nc)
  # Geom type
  tterra_header("Geometry type: ", tools::toTitleCase(terra::geomtype(x)))
  # CRS info
  tterra_header_string_crs(x)
  # Extent info
  tterra_header_string_ext(x)

  # Body
  cli::cat_line() # Empty line

  if (ncol(x) == 0) {
    cli::cat_line("SpatVector with no attributes (only geometries)")
  } else {
    # Manipulate tibble format (with options if provided)
    tterra_body(x, ..., width = width, n = n, max_extra_cols = max_extra_cols)
  }
  invisible(x)
}


#' @export
dplyr::glimpse


# Helpers ----

get_named_crs <- function(x) {
  # Based in terra:::.name_or_proj4()
  pulled <- pull_crs(x)
  if (is.na(pulled)) {
    return(NA)
  }

  d <- try(terra::crs(pulled, describe = TRUE), silent = TRUE)

  # nocov start
  if (inherits(d, "try-error")) {
    return(NA)
  }
  # nocov end

  r <- terra::crs(pulled, proj = TRUE)

  # nocov start
  if (inherits(r, "try-error")) {
    return(NA)
  }
  # nocov end

  if (!(d$name %in% c(NA, "unknown", "unnamed"))) {
    if (startsWith(r, "+proj=longlat")) {
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
  if (is.na(r) || r == "" || is.null(r)) {
    r <- NA
  }
  # nocov end
  r
}


# To convert lon lat from decimal to pretty
decimal_to_degrees <- function(x, type = c("lon", "lat", "null")) {
  type <- match.arg(type)
  coordinit <- x
  x <- abs(x)
  x_int <- as.integer(x)
  m <- (x - x_int) * 60
  m_int <- as.integer(m)
  s <- round((m - m_int) * 60, 2)

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
    label <- paste0(x_int, "\u00b0 ", m_int, "' ", s, '\" ', lab)
  } else {
    label <- paste0(
      c(x_int, m_int, s),
      c("\u00b0", "'", '\"')
    )
    label <- label[c(x_int, m_int, s) != 0]
    label <- paste0(label, collapse = " ")
  }

  label
}


# Main style
tterra_head_style <- cli::make_ansi_style(grey(0.6), grey = TRUE)

tterra_header <- function(...) {
  fmted <- paste("# ", paste0(..., collapse = " "))

  cli::cat_line(tterra_head_style(fmted))
}

# For CRS
tterra_header_string_crs <- function(x) {
  crsnamed <- get_named_crs(x)
  if (is.na(crsnamed)) {
    tterra_header("CRS: Not Defined / Empty")
    return(invisible(NULL))
  }
  pulled_crs <- pull_crs(x)
  if (sf::st_is_longlat(pulled_crs)) {
    tterra_header("Geodetic CRS: ", crsnamed)
    return(invisible(NULL))
  }

  unts <- try(sf::st_crs(pulled_crs)$units, silent = TRUE)
  # Inform of units
  tterra_header("Projected CRS: ", crsnamed)

  if (inherits(unts, "character")) {
    unitsdb <- unitsdb

    longname <- as.vector(unitsdb[unitsdb$abb == unts, ]$name)

    tterra_header("CRS projection units: ", longname, " <", unts, ">")
  }
}

# For extent
tterra_header_string_ext <- function(x) {
  ext <- as.vector(terra::ext(x))
  is_lonlat <- sf::st_is_longlat(pull_crs(x))

  if (isTRUE(is_lonlat)) {
    lons <- lapply(ext[c("xmin", "xmax")], decimal_to_degrees, type = "lon")

    lats <- lapply(ext[c("ymin", "ymax")], decimal_to_degrees, type = "lat")

    ext_fmt <- unlist(c(lons, lats))
  } else {
    ext_fmt <- format(
      ext,
      big.mark = ",",
      decimal.mark = ".",
      justify = "right"
    )
  }

  xfmt <- paste(ext_fmt[c("xmin", "xmax")], collapse = " / ")
  yfmt <- paste(ext_fmt[c("ymin", "ymax")], collapse = " / ")

  extnamed <- paste0("([", xfmt, "] , [", yfmt, "])")

  tterra_header("Extent (x / y) : ", extnamed)
}

tterra_header_string_res <- function(x) {
  if (isTRUE(sf::st_is_longlat(pull_crs(x)))) {
    rs <- lapply(terra::res(x), decimal_to_degrees, type = "null")
    rs <- paste0(unlist(rs), collapse = " , ")
  } else {
    rs <- paste(
      format(terra::res(x), big.mark = ",", decimal.mark = "."),
      collapse = " / "
    )
  }
  tterra_header("Resolution (x / y): (", rs, ")")
}

tterra_header_string_rgb <- function(x) {
  # Check RGB color
  rgb_info <- terra::RGB(x)
  if (!all(!is.null(rgb_info), length(rgb_info) > 0)) {
    return(invisible(NULL))
  }

  # Make msg
  ch_name <- names(x)[rgb_info]
  nm <- c("Red", "Green", "Blue", "Alpha")[seq_along(rgb_info)]
  ch_end <- paste0(ch_name, " (", nm, ")", collapse = ", ")
  pl <- ifelse(length(rgb_info) == 1, "channel", "channels")

  tterra_header(
    "SpatRaster with ",
    length(rgb_info),
    " RGB ",
    pl,
    ": ",
    ch_end
  )
}

tterra_header_string_ctab <- function(x) {
  # Check coltab
  coltab_info <- terra::has.colors(x)
  if (!any(coltab_info)) {
    return(invisible(NULL))
  }

  lcol <- length(coltab_info[coltab_info])
  pl <- ifelse(lcol == 1, "table", "tables")

  ch_name <- names(x)[coltab_info]
  ch_end <- paste0(ch_name, collapse = ", ")

  tterra_header("SpatRaster with ", lcol, " color ", pl, " in: ", ch_end)
}

# Body from tbl
tterra_body <- function(
  x,
  width = cli::console_width(),
  n = 10,
  ...,
  max_extra_cols = 20
) {
  init_type <- class(x)
  # Need just a small subset for printing, improve speed
  max_rows <- min(terra::nrow(x), 30)

  x <- as_tibble(x[seq_len(max_rows), ], ...)
  if (!is.numeric(n)) {
    n <- 10
  }
  n <- max(1, n)

  if (!is.numeric(max_extra_cols)) {
    n <- 20
  }
  max_extra_cols <- max(1, max_extra_cols)

  extra_cols <- NULL

  if (terra::ncol(x) <= n) {
    col_sel <- x
  } else {
    col_sel <- x[, seq_len(n)]
    extra_cols <- x[, setdiff(names(x), names(col_sel))]
  }

  # Main body via glimpse
  capt <- utils::capture.output(dplyr::glimpse(col_sel, width = width))
  cli::cat_line(capt[-c(1:2)])

  # Make footer
  if (!is.null(extra_cols)) {
    extra_text <- vapply(
      extra_cols,
      function(x) {
        if (requireNamespace("vctrs", quietly = TRUE)) {
          aa <- paste0(vctrs::vec_ptype_abbr(x), collapse = "/")
        } else {
          # nocov start
          aa <- paste0(class(x), collapse = "/")
          # nocov end
        }

        paste0("<", aa, ">")
      },
      character(1)
    )

    # Check if we hit extra cols

    dots_extra <- ""
    if (length(extra_text) > max_extra_cols) {
      extra_text <- extra_text[seq_len(max_extra_cols)]
      dots_extra <- ", ..."
    }

    extra_text <- paste(names(extra_text), extra_text, collapse = ", ")
    extra_text <- paste0(extra_text, dots_extra)
    # Full message
    nms <- ifelse(ncol(extra_cols) == 1, "variable", "variables")
    if (init_type == "SpatRaster") {
      nms <- gsub("variable", "layer", nms, fixed = TRUE)
    }
    full <- paste(
      "#",
      cli::symbol$info,
      format(ncol(extra_cols), big.mark = ".", decimal.mark = ","),
      "more",
      nms,
      ":",
      extra_text
    )

    full <- cli::ansi_strwrap(full, exdent = 3)
    cli::cat_line(tterra_head_style(full))

    nm2 <- ifelse(init_type == "SpatVector", "columns", "layers")

    hint <- paste0(
      "# ",
      cli::symbol$info,
      " Use `tidyterra::glimpse(n = ...)` to see more ",
      nm2
    )

    hint <- cli::ansi_strwrap(hint, exdent = 3)
    cli::cat_line(tterra_head_style(hint))
  }
}
