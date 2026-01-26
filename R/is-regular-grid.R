#' Check if x and y positions conforms a regular grid
#'
#' @description
#'
#' Assess if the coordinates x,y of an object conforms a regular grid. This
#' function is called by its side effects.
#'
#' This function is internally called by [as_spatraster()].
#'
#' @export
#'
#' @return
#' `invisible()` if is regular or an error message otherwise
#'
#' @param xy A matrix, data frame or tibble of at least two columns representing
#'   x and y coordinates.
#' @inheritParams as_spatraster
#'
#' @seealso [as_spatraster()]
#'
#' @family helpers
#'
#' @examples
#'
#' p <- matrix(1:90, nrow = 45, ncol = 2)
#'
#' is_regular_grid(p)
#'
#' # Jitter location
#' set.seed(1234)
#' jitter <- runif(length(p)) / 10e4
#' p_jitter <- p + jitter
#'
#' # Need to adjust digits
#' is_regular_grid(p_jitter, digits = 4)
#'
is_regular_grid <- function(xy, digits = 6) {
  # From https://github.com/rspatial/terra/blob/master/R/rast.R

  # Need to work out with tibbles

  xy_df <- as.data.frame(xy)

  newdf <- data.frame(
    x = as.double(xy_df[, 1]),
    y = as.double(xy_df[, 2])
  )

  xyz <- as.matrix(newdf)
  xyz <- matrix(as.numeric(xyz), ncol = ncol(xyz), nrow = nrow(xyz))

  x <- sort(unique(xyz[, 1]))
  dx <- x[-1] - x[-length(x)]

  rx <- min(dx)
  for (i in 1:5) {
    rx <- rx / i
    q <- sum(round(dx / rx, digits = digits) %% 1)
    if (q == 0) {
      break
    }
  }
  if (q > 0) {
    cli::cli_abort(paste(
      "{.arg x} cell sizes are not regular.",
      "
      Try with a lower {.arg digit} value."
    ))
  }

  y <- sort(unique(xyz[, 2]))
  dy <- y[-1] - y[-length(y)]
  ry <- min(dy)
  for (i in 1:5) {
    ry <- ry / i
    q <- sum(round(dy / ry, digits = digits) %% 1)
    if (q == 0) {
      break
    }
  }
  if (q > 0) {
    cli::cli_abort(paste(
      "{.arg y} cell sizes are not regular. Try with a lower",
      "{.arg digit}",
      "value."
    ))
  }

  invisible()
}
