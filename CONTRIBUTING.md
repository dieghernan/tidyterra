# Contributing to tidyterra

This guide explains how to propose a change to **tidyterra**. Review the
[development contributing guide](https://rstd.io/tidy-contrib) and [code
review principles](https://code-review.tidyverse.org/) before submitting
a substantial contribution.

## Fixing typos

You can fix typos, spelling mistakes or grammatical errors in the
documentation directly in the GitHub web interface, as long as you make
the changes in the *source* file. This generally means editing
[**roxygen2**
comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`
file, not an `.Rd` file. You can find the `.R` file that generates the
`.Rd` file by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, first file an issue and make sure
someone from the team agrees that it is needed. If you have found a bug,
file an issue that illustrates it with a minimal
[**reprex**](https://www.tidyverse.org/help/#reprex). This will also
help you write a unit test, if needed. See our guide on [how to create a
great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

- Fork the package and clone it onto your computer. If you have not done
  this before, use
  `usethis::create_from_github("dieghernan/tidyterra", fork = TRUE)`.
- Install all development dependencies with
  `devtools::install_dev_deps()`, then make sure the package passes
  `R CMD check` by running `devtools::check()`. If `R CMD check` does
  not pass cleanly, ask for help before continuing.
- Create a Git branch for your pull request (PR). Use
  `usethis::pr_init("brief-description-of-change")`.
- Make your changes, commit them to Git, then create a PR by running
  `usethis::pr_push()` and following the prompts in your browser. The PR
  title should briefly describe the change. The PR body should contain
  `Fixes #issue-number`.
- For user-facing changes, add a bullet to the top of `NEWS.md`, just
  below the first heading. Follow the style at
  <https://style.tidyverse.org/news.html>.

### Code style

- New code should follow the [**tidyverse** style
  guide](https://style.tidyverse.org). You can use
  [**Air**](https://posit-dev.github.io/air/) to apply this style, but
  do not restyle code unrelated to your PR.
- We use [**roxygen2**](https://cran.r-project.org/package=roxygen2)
  with [Markdown
  syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html)
  for documentation.
- We use [**testthat**](https://cran.r-project.org/package=testthat) for
  unit tests. Contributions that include test cases are easier to
  accept.

## Code of Conduct

Please note that the **tidyterra** project is released with a
[Contributor Code of
Conduct](https://dieghernan.github.io/tidyterra/CODE_OF_CONDUCT.md). By
contributing to this project you agree to abide by its terms.
