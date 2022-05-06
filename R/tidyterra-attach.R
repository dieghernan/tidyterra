# From https://github.com/tidyverse/tidyverse/blob/main/R/attach.R

# nocov start

core <- c("tibble", "tidyr", "dplyr")

core_unloaded <- function() {
  search <- paste0("package:", core)
  core[!search %in% search()]
}
# Attach the package from the same package library it was
# loaded from before. https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
  load_silent <- function(pkg) {
    loc <- if (pkg %in% loadedNamespaces()) {
      dirname(getNamespaceInfo(
        pkg,
        "path"
      ))
    }
    suppressWarnings(
      library(pkg,
        lib.loc = loc, character.only = TRUE,
        warn.conflicts = FALSE
      )
    )
  }
  do.call(
    "load_silent",
    list(pkg)
  )
}

tidyterra_attach <- function() {
  to_load <- core_unloaded()
  if (length(to_load) == 0) {
    return(invisible())
  }

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("tidyterra ", package_version("tidyterra"))
    ),
    startup = TRUE
  )
  msg(crayon::green(
    paste(
      "\nSuppress this startup message by setting",
      "Sys.setenv(tidyterra.quiet = TRUE)\n"
    )
  ))

  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(
      as.character(version[4:length(version)])
    )
  }
  paste0(version, collapse = ".")
}


msg <- function(..., startup = FALSE, silent = attach_silent()) {
  if (silent) {
    return(invisible())
  }

  if (startup) {
    if (!attach_silent()) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}


text_col <- function(x) {
  return(x)
}


attach_silent <- function() {
  silent <- isTRUE((Sys.getenv("tidyterra.quiet")) == "TRUE")

  return(silent)
}


# nocov end
