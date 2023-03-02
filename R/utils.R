across_all_of <- function(vars) {
  dplyr::across(dplyr::all_of(vars))
}

as_spat_internal <- function(x) {
  if (any(inherits(x, "SpatRaster"),
          isTRUE(attr(x, "source") == "SpatRaster"))) {
    return(as_spatrast_attr(x))
  } else if (any(inherits(x, "SpatVector"),
                 isTRUE(attr(x, "source") == "SpatVector"))) {
    return(as_spatvect_attr(x))
  }


  cli::cli_abort("Can't convert x back to a Spat* object. Something went wrong")
}
