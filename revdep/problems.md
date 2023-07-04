# sparrpowR

<details>

* Version: 0.2.7
* GitHub: https://github.com/machiela-lab/sparrpowR
* Source code: https://github.com/cran/sparrpowR
* Date/Publication: 2023-02-02 01:00:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::revdep_details(, "sparrpowR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'spelling.R'
      Comparing 'spelling.Rout' to 'spelling.Rout.save' ...6,97d5
    < Potential spelling errors:
    <   WORD              FOUND IN
    < Baddeley          NEWS.md:17,42
    < Benjamini         jitter_power.Rd:87
    <                   pval_correct.Rd:31
    <                   spatial_power.Rd:143
    < Bithell           sparrpowR-package.Rd:12
    <                   description:5,7
    ...
       1. ├─testthat::expect_named(...) at test-test-spatial_power.R:154:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─sparrpowR::spatial_power(...)
       5.   └─... %fun% ...
       6.     └─e$fun(obj, substitute(ex), parent.frame(), e$data)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

