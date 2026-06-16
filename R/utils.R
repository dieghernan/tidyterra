# Reconstruction helpers ----
as_spat_internal <- function(x) {
  if (
    any(inherits(x, "SpatRaster"), isTRUE(attr(x, "source") == "SpatRaster"))
  ) {
    return(as_spatrast_attr(x))
  } else if (
    any(inherits(x, "SpatVector"), isTRUE(attr(x, "source") == "SpatVector"))
  ) {
    return(as_spatvect_attr(x))
  }

  cli::cli_abort(paste(
    "Cannot convert {.arg x} back to a {.cls Spat*} object.",
    "Required reconstruction attributes are missing."
  ))
}

# Restore missing attributes from a template object.
restore_attr <- function(x, template) {
  init_attr <- attributes(x)
  temp_attr <- attributes(template)

  addatt <- setdiff(names(temp_attr), names(init_attr))

  finalattr <- c(init_attr, temp_attr[addatt])

  attributes(x) <- finalattr

  x
}

# Selection and name helpers ----
across_all_of <- function(vars) {
  dplyr::across(dplyr::all_of(vars))
}

#' Create a safe index name
#' @param x Base string used to create the index name.
#' @param y Object whose names, as returned by `names(y)`, are checked.
#' @noRd
make_safe_index <- function(x, y) {
  nams <- names(y)

  i <- 1

  init_x <- x

  is_safe <- !(x %in% nams)

  while (isFALSE(is_safe)) {
    x <- paste0(init_x, "_", sprintf("%03d", i))
    i <- i + 1
    is_safe <- !(x %in% nams)
  }

  x
}

# Validation and message helpers ----
check_alpha <- function(alpha, call = rlang::caller_env()) {
  if (alpha < 0 || alpha > 1) {
    cli::cli_abort(
      "{.arg alpha} must be between {.val {0}} and {.val {1}}.",
      call = call
    )
  }

  invisible(NULL)
}

check_alpha_direction <- function(
  alpha,
  direction,
  call = rlang::caller_env()
) {
  check_alpha(alpha, call = call)

  if (!direction %in% c(-1, 1)) {
    cli::cli_abort(
      "{.arg direction} must be either {.val {1}} or {.val {-1}}.",
      call = call
    )
  }

  invisible(NULL)
}

check_spatraster <- function(data, fn, call = rlang::caller_env()) {
  if (!inherits(data, "SpatRaster")) {
    cli::cli_abort(
      paste(
        "{.fun tidyterra::{fn}} only works with",
        "{.cls SpatRaster} objects, not {.cls {class(data)}}.",
        "See {.help terra::rast}."
      ),
      call = call
    )
  }

  invisible(NULL)
}

check_palette <- function(
  palette,
  choices,
  help = NULL,
  call = rlang::caller_env()
) {
  if (palette %in% choices) {
    return(invisible(NULL))
  }

  if (is.null(help)) {
    cli::cli_abort("{.arg palette} is not a known palette.", call = call)
  }

  cli::cli_abort(
    paste(
      "{.arg palette} {.val {palette}} is not a known palette.",
      "See {.help {help}}."
    ),
    call = call
  )
}

abort_lost_geometry_after_pivot <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    paste0(
      "Cannot rebuild the {.cls SpatVector}. ",
      "The {.val geometry} column was lost after pivoting."
    ),
    call = call
  )
}

warn_overlapping_layers <- function(data, fn) {
  if (length(unique(data$PANEL)) == length(unique(data$lyr))) {
    return(invisible(NULL))
  }

  nly <- length(unique(data$lyr))
  if (nly <= 1) {
    return(invisible(NULL)) # nolint
  }

  cli::cli_alert_warning(paste(
    "{.fun tidyterra::{fn}}:",
    "Plotting {.val {nly}} overlapping layer{?s}:",
    "{.val {unique(data$lyr)}}. Either:"
  ))
  cli::cli_bullets(c(
    "*" = "Use {.code facet_wrap(~lyr)} to facet layers.",
    "*" = paste0(
      "Use {.code aes(fill = <name_of_layer>)} ",
      "to display a single layer."
    )
  ))

  invisible(NULL)
}

# Scale helpers ----
gradient_pal <- function(pal, n) {
  scales::gradient_n_pal(pal(n))
}

discrete_pal_scale <- function(
  aesthetics,
  palette,
  ...,
  na.translate = FALSE,
  drop = TRUE
) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = palette,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

pal_discrete_scale <- function(
  aesthetics,
  palette,
  alpha,
  direction,
  ...,
  na.translate,
  drop,
  call = rlang::caller_env()
) {
  check_alpha_direction(alpha, direction, call = call)

  discrete_pal_scale(
    aesthetics,
    palette,
    na.translate = na.translate,
    drop = drop,
    ...
  )
}

pal_gradient_scale <- function(
  scale,
  aesthetics,
  palette,
  n,
  alpha,
  direction,
  ...,
  na.value,
  guide,
  call = rlang::caller_env()
) {
  check_alpha_direction(alpha, direction, call = call)

  if (is.function(n)) {
    n <- n()
  }

  scale(
    aesthetics = aesthetics,
    palette = gradient_pal(palette, n),
    na.value = na.value,
    guide = guide,
    ...
  )
}

tint_scale_params <- function(
  coltab,
  palette,
  alpha,
  direction,
  values,
  limits,
  help,
  call = rlang::caller_env()
) {
  check_palette(palette, coltab$pal, help = help, call = call)

  pal_cols <- coltab[coltab$pal == palette, ]
  colors <- as.character(pal_cols$hex)
  if (direction == -1) {
    colors <- rev(colors)
  }
  if (alpha != 1) {
    colors <- ggplot2::alpha(colors, alpha = alpha)
  }

  if (is.null(values)) {
    values <- pal_cols$limit
  }
  if (is.null(limits)) {
    limits <- range(values)
  }

  list(
    colors = colors,
    values = scales::rescale(values, from = limits),
    limits = limits
  )
}
