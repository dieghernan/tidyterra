list(
  # Family convention:
  # - `<package>.methods` groups S3 methods by upstream package.
  # - `<package>.<topic>` groups method families shown as pkgdown subsections.
  # - unqualified families group tidyterra helpers, datasets and palettes.
  # Keep these names aligned with `@family` tags and pkgdown `has_concept()`.
  rd_family_title = list(
    coerce = "Coercing objects:",
    helpers = "Other helpers:",
    datasets = "Other datasets:",
    tibble.methods = "Other \\CRANpkg{tibble} methods:",
    generics.methods = "Other \\CRANpkg{generics} methods:",
    dplyr.methods = "Other \\CRANpkg{dplyr} methods:",
    dplyr.rows = "Other \\CRANpkg{dplyr} verbs that operate on rows:",
    dplyr.cols = "Other \\CRANpkg{dplyr} verbs that operate on columns:",
    dplyr.groups = paste0(
      "Other \\CRANpkg{dplyr} verbs that operate on ",
      "groups of rows:"
    ),
    dplyr.group_functions = "Other \\CRANpkg{dplyr} grouping methods:",
    dplyr.pairs = paste0(
      "Other \\CRANpkg{dplyr} verbs that operate on ",
      "pairs of \\code{SpatVector} and data frame objects:"
    ),
    dplyr.single_table = "Other \\CRANpkg{dplyr} single-table verbs:",
    tidyr.methods = "Other \\CRANpkg{tidyr} methods:",
    tidyr.character = "Other \\CRANpkg{tidyr} character methods:",
    tidyr.nest = "Other \\CRANpkg{tidyr} nesting methods:",
    tidyr.pivot = "Other \\CRANpkg{tidyr} verbs for pivoting:",
    tidyr.rows = "Other \\CRANpkg{tidyr} verbs for rows:",
    tidyr.missing = paste0(
      "Other \\CRANpkg{tidyr} verbs for handling ",
      "missing values:"
    ),
    ggplot2.utils = "Other \\CRANpkg{ggplot2} helpers:",
    ggplot2.methods = "Other \\CRANpkg{ggplot2} methods:",
    joins = "Other joins:",
    grouping = "Other grouping methods:",
    gradients = "Other color scales, palettes and hypsometric tints:"
  )
)
