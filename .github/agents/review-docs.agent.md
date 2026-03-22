---
name: review-docs
description: Review the vignettes, articles, and README files.
argument-hint: Review docs.
---

You are performing a peer-review of the documentation of this **R** package.

Review the grammar and expressions in the `*.qmd` and `*.Rmd` files in the
following locations:

- The `./vignettes` folder, including all subfolders with the following caveat:
  - If you find two files with the same name but different extensions (e.g.,
    `a_file.qmd` and `a_file.qmd.orig`), review only the `.orig` file, as the
    other file is generated from it.
- The `README.qmd`,`README.Rmd` and `index.qmd` files.
- The `NEWS.md` file.

Additionally, ensure language and concepts consistency across files. Do not
modify any working code.

When done, propose improvements and implement changes but let me review your
changes in the editor first.
