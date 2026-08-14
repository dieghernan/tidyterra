# Local visual tests

**This folder is `.Rbuildignored`**.

This folder contains large local-only tests for visual snapshot inspection. They
complement the regular `tests/testthat` suite, but are not included in the
package bundle because the SVG snapshots are too large.

Run the whole local suite with:

``` r
devtools::load_all()
testthat::local_on_cran(FALSE)

testthat::test_dir(
  "tests/testthat/test_local",
  package = "tidyterra",
  load_package = "source"
)

withr::deferred_clear()
```

Run one local file with:

``` r
devtools::load_all()
testthat::local_on_cran(FALSE)
source("tests/testthat/test_local/helper-local.R")

testthat::test_file(
  "tests/testthat/test_local/test-autoplot-Spat.R",
  package = "tidyterra",
  load_package = "source"
)

withr::deferred_clear()
```

## Organization

- Keep visual tests in `test-{feature}.R` files.
- Keep non-visual behavioral checks in the regular `tests/testthat` suite when
  they do not require large SVG snapshots.
- Prefer several focused `test_that()` blocks over one long block with many
  unrelated snapshots.
- Put shared fixture constructors in `helper-local.R`.

## Snapshot names

Use stable, sortable names:

``` text
{group}_{NN}: {scenario}
{group}_{NN}{suffix}: {scenario}
```

Examples:

- `norgb_01: regular`
- `rgb_02: with opts`
- `coltab_05: force no facets`
- `crs_01a: regular no facet`

Rules:

- Use two-digit numeric prefixes (`01`, `02`, ..., `10`).
- Use lowercase scenario text.
- Use the same separator style within a file.
- Keep the snapshot name specific enough to understand the visual scenario
  without opening the test body.
