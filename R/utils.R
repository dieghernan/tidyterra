across_all_of <- function(vars) {
  dplyr::across(dplyr::all_of(vars))
}

as_spat_internal <- function(x) {
  if (any(
    inherits(x, "SpatRaster"),
    isTRUE(attr(x, "source") == "SpatRaster")
  )) {
    return(as_spatrast_attr(x))
  } else if (any(
    inherits(x, "SpatVector"),
    isTRUE(attr(x, "source") == "SpatVector")
  )) {
    return(as_spatvect_attr(x))
  }


  cli::cli_abort(paste(
    "Can't convert {.arg x} back to a {.cls Spat*} object.",
    "Something went wrong"
  ))
}

# Restore attributes from template
restore_attr <- function(x, template) {
  init_attr <- attributes(x)
  temp_attr <- attributes(template)

  addatt <- setdiff(names(temp_attr), names(init_attr))

  finalattr <- c(init_attr, temp_attr[addatt])

  attributes(x) <- finalattr

  x
}

#' Create safe index name
#' @param x pattern to create
#' @param y object with names (Can perform `names(y)`) to assess.
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

  return(x)
}
