#' Keep distinct/unique rows and geometries of SpatVector objects
#'
#' @description
#' Keep only unique/distinct rows and geometries from a SpatVector.
#'
#' @export
#' @rdname distinct.SpatVector
#' @name distinct.SpatVector
#'
#' @seealso [dplyr::distinct()], [terra::unique()]
#'
#' @family dplyr.methods
#' @family single table verbs
#'
#' @importFrom dplyr distinct
#'
#' @param .data A SpatVector created with [terra::vect()].
#' @param ... <[`data-masking`][dplyr::distinct]> Optional variables to
#'   use when determining uniqueness. If there are multiple rows for a given
#'   combination of inputs, only the first row will be preserved. If omitted,
#'   will use all variables in the data frame. There is a special variable
#'   name, `geometry`, that would remove duplicate geometries. See **Methods**.
#' @inheritParams dplyr::distinct
#' @return A SpatVector object.
#'
#'
#' @section terra equivalent:
#'
#' [terra::unique()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::distinct()] function.
#'
#' ## SpatVector
#'
#' The geometry column has a sticky behavior. This means that when passing
#' variables to `...` the geometry information would always be always
#' include on the evaluation.
#'
#' It is possible to remove duplicate geometries including the `geometry`
#' variable explicitly in the `...` call. See **Examples**.
#'
#'
#' @examples
#'
#' library(terra)
#'
#' # Create a vector from data frame
#' df <- data.frame(
#'   A = sample(10, 100, rep = TRUE),
#'   B = sample(10, 100, rep = TRUE),
#'   lon = sample(c(0, 20), 100, replace = TRUE),
#'   lat = sample(c(20, 0), 100, replace = TRUE)
#' )
#'
#' v <- vect(df, crs = pull_crs(4326))
#'
#' v
#' nrow(v)
#'
#'
#' # All duplicates
#' ex1 <- distinct(v)
#' ex1
#'
#' nrow(ex1)
#'
#' # Duplicates by A
#' ex2 <- distinct(v, A)
#' ex2
#'
#' nrow(ex2)
#'
#' # Same but keeping all cols
#' ex2b <- distinct(v, A, .keep_all = TRUE)
#' ex2b
#' nrow(ex2b)
#'
#'
#' # Unique geometries
#' ex3 <- distinct(v, geometry)
#'
#' ex3
#' nrow(ex3)
#' # Same as terra::unique()
#' terra::unique(ex3)
distinct.SpatVector <- function(.data, ..., .keep_all = FALSE) {
  # Own implementation
  init_names <- names(.data)
  newnames <- make_col_names(.data, geom = "WKT", messages = FALSE)
  end_names <- names(newnames)[seq_len(length(init_names))]
  def <- as_tibble(.data, geom = "WKT")

  keep_name_list <- list(
    init = init_names,
    after = end_names,
    changed = init_names == end_names
  )

  # Make geometry sticky
  if (rlang::dots_n(...) > 0) {
    dist <- dplyr::distinct(def, ..., .data$geometry, .keep_all = .keep_all)
  } else {
    # If no dots then pass directly
    dist <- dplyr::distinct(def, ..., .keep_all = .keep_all)
  }

  # Regenerate
  reg <- terra::vect(dist, geom = "geometry", crs = attr(dist, "crs"))


  # Regenerate names
  after_names <- names(reg)
  int <- intersect(end_names, after_names)
  if (length(int) > 0) {
    # Get vector of changes
    vend <- match(int, keep_name_list$after)
    vlogi <- keep_name_list$changed[vend]
    if (any(vlogi == FALSE)) {
      message(cli::col_black("Regenerating column names:"))

      to_change <- keep_name_list$init[vend]

      message(cli::col_black(
        paste0("`", int, "` -> `", to_change, "`", collapse = "\n")
      ))
      message(cli::col_black("\n"))

      names(reg) <- to_change
    }
  }

  return(reg)
}



#' @export
dplyr::distinct
