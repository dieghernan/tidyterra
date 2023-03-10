


bind_spat_cols <- function(...,
                           .name_repair = c(
                             "unique", "universal",
                             "check_unique", "minimal"
                           )) {
  dots <- rlang::list2(...)

  # Return empty on none
  if (length(dots) == 0) {
    return(terra::vect("POINT EMPTY"))
  }

  # Make it work with list
  if (length(dots) == 1 && is.list(dots[[1]])) {
    # If is a list unlist the first level
    dots <- dots[[1]]
  }

  # Checks
  # Ensure first is SpatVector
  if (!inherits(dots[[1]], "SpatVector")) {
    cli::cli_abort(paste(
      "Object #1 in",
      cli::col_blue("..."), "is not a",
      cli::col_blue("SpatVector")
    ))
  }

  # Get templates
  template <- dots[[1]]

  # Ensure all are tibbles
  alltibbs <- lapply(seq_len(length(dots)), function(i) {
    x <- dots[[i]]

    # First is always a SpatVector
    if (i == 1) {
      return(as_tbl_internal(x))
    }

    # Rest of cases

    if (inherits(x, "SpatVector")) {
      return(as_tibble(x))
    }


    if (inherits(x, "sf")) {
      return(sf::st_drop_geometry(x))
    }

    return(x)
  })


  endobj <- dplyr::bind_cols(alltibbs, .name_repair = .name_repair)

  # Restore attribs
  endobj <- restore_attr(endobj, alltibbs[[1]])


  # To SpatVector
  sp <- as_spat_internal(endobj)

  # Groups
  sp <- group_prepare_spat(sp, template)

  return(sp)
}
