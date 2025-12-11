# Test on local installation

**This folder is `.Rbuildignored`**.

Test locally snapshots. It can be run with:

``` r

# Load package
devtools::load_all()

testthat::local_on_cran(FALSE)

testthat::test_dir("tests/testthat/test_local", 
                   package = "tidyterra", 
                   load_package = "source"
                   )

withr::deferred_clear()
```
