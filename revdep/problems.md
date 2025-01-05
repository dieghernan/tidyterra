# ebvcube

<details>

* Version: 0.3.3
* GitHub: https://github.com/EBVcube/ebvcube
* Source code: https://github.com/cran/ebvcube
* Date/Publication: 2024-12-17 10:20:06 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::revdep_details(, "ebvcube")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
      > test_check("ebvcube")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 225 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-analyse.R:22:3'): test ebv_analyse bb ──────────────────────────
      Error in `ebv_i_check_ram(c(ncol, nrow), timestep, entity, prop@ebv_cube$type)`: Reading that data into memory will significantly slow down your PC. If you still want to go on, set ignore_RAM = TRUE.
      Backtrace:
          ▆
       1. └─ebvcube::ebv_analyse(...) at test-analyse.R:22:3
       2.   └─ebvcube::ebv_read_bb(...)
       3.     └─ebvcube:::ebv_i_check_ram(c(ncol, nrow), timestep, entity, prop@ebv_cube$type)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 225 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

