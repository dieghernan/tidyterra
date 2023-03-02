across_all_of <- function(vars) {
  dplyr::across(dplyr::all_of(vars))
}
