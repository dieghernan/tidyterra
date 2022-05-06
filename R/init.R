# nocov start

.onLoad <- function(libname, pkgname) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  crayon::num_colors(TRUE)
  tidyterra_attach()
}


is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

# nocov end
