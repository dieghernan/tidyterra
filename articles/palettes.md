# Gradient palettes in tidyterra

This page shows a gallery of maps that can be displayed with the
gradient fill scales included with **tidyterra**. A basic code to
generate this map is:

``` r
library(tidyterra)
library(terra)
library(ggplot2)

r <- rast(system.file("extdata/volcano2.tif", package = "tidyterra"))

ggplot() +
  geom_spatraster(data = r) +
  # Use the palette you like, in this case:
  scale_fill_hypso_c() +
  theme_void()
```

## `scale_fill_terrain_*` and `scale_fill_wiki_*`

Check
[`scale_fill_terrain_c()`](https://dieghernan.github.io/tidyterra/reference/scale_terrain.md)
and
[`scale_fill_wiki_c()`](https://dieghernan.github.io/tidyterra/reference/scale_wiki.md)
for more info.

![](palettes_files/figure-html/terr_wiki-1.png)![](palettes_files/figure-html/terr_wiki-2.png)

## `scale_fill_whitebox_*`

Check
[`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/reference/scale_whitebox.md)
for more info.

![](palettes_files/figure-html/whitebox-1.png)![](palettes_files/figure-html/whitebox-2.png)![](palettes_files/figure-html/whitebox-3.png)![](palettes_files/figure-html/whitebox-4.png)![](palettes_files/figure-html/whitebox-5.png)![](palettes_files/figure-html/whitebox-6.png)![](palettes_files/figure-html/whitebox-7.png)![](palettes_files/figure-html/whitebox-8.png)![](palettes_files/figure-html/whitebox-9.png)![](palettes_files/figure-html/whitebox-10.png)![](palettes_files/figure-html/whitebox-11.png)

## `scale_fill_hypso_*`

Check
[`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/reference/scale_hypso.md)
for more info.

![](palettes_files/figure-html/hypso-1.png)![](palettes_files/figure-html/hypso-2.png)![](palettes_files/figure-html/hypso-3.png)![](palettes_files/figure-html/hypso-4.png)![](palettes_files/figure-html/hypso-5.png)![](palettes_files/figure-html/hypso-6.png)![](palettes_files/figure-html/hypso-7.png)![](palettes_files/figure-html/hypso-8.png)![](palettes_files/figure-html/hypso-9.png)![](palettes_files/figure-html/hypso-10.png)![](palettes_files/figure-html/hypso-11.png)![](palettes_files/figure-html/hypso-12.png)![](palettes_files/figure-html/hypso-13.png)![](palettes_files/figure-html/hypso-14.png)![](palettes_files/figure-html/hypso-15.png)![](palettes_files/figure-html/hypso-16.png)![](palettes_files/figure-html/hypso-17.png)![](palettes_files/figure-html/hypso-18.png)![](palettes_files/figure-html/hypso-19.png)![](palettes_files/figure-html/hypso-20.png)![](palettes_files/figure-html/hypso-21.png)![](palettes_files/figure-html/hypso-22.png)![](palettes_files/figure-html/hypso-23.png)![](palettes_files/figure-html/hypso-24.png)![](palettes_files/figure-html/hypso-25.png)![](palettes_files/figure-html/hypso-26.png)![](palettes_files/figure-html/hypso-27.png)![](palettes_files/figure-html/hypso-28.png)![](palettes_files/figure-html/hypso-29.png)![](palettes_files/figure-html/hypso-30.png)![](palettes_files/figure-html/hypso-31.png)![](palettes_files/figure-html/hypso-32.png)![](palettes_files/figure-html/hypso-33.png)

## `scale_fill_cross_blended_*`

Check
[`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/reference/scale_cross_blended.md)
for more info.

![](palettes_files/figure-html/cross-1.png)![](palettes_files/figure-html/cross-2.png)![](palettes_files/figure-html/cross-3.png)![](palettes_files/figure-html/cross-4.png)

## `scale_fill_grass_*`

Check
[`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/reference/scale_grass.md)
for more info. Plots produced using `use_grass_range = FALSE`.

![](palettes_files/figure-html/grass-1.png)![](palettes_files/figure-html/grass-2.png)![](palettes_files/figure-html/grass-3.png)![](palettes_files/figure-html/grass-4.png)![](palettes_files/figure-html/grass-5.png)![](palettes_files/figure-html/grass-6.png)![](palettes_files/figure-html/grass-7.png)![](palettes_files/figure-html/grass-8.png)![](palettes_files/figure-html/grass-9.png)![](palettes_files/figure-html/grass-10.png)![](palettes_files/figure-html/grass-11.png)![](palettes_files/figure-html/grass-12.png)![](palettes_files/figure-html/grass-13.png)![](palettes_files/figure-html/grass-14.png)![](palettes_files/figure-html/grass-15.png)![](palettes_files/figure-html/grass-16.png)![](palettes_files/figure-html/grass-17.png)![](palettes_files/figure-html/grass-18.png)![](palettes_files/figure-html/grass-19.png)![](palettes_files/figure-html/grass-20.png)![](palettes_files/figure-html/grass-21.png)![](palettes_files/figure-html/grass-22.png)![](palettes_files/figure-html/grass-23.png)![](palettes_files/figure-html/grass-24.png)![](palettes_files/figure-html/grass-25.png)![](palettes_files/figure-html/grass-26.png)![](palettes_files/figure-html/grass-27.png)![](palettes_files/figure-html/grass-28.png)![](palettes_files/figure-html/grass-29.png)![](palettes_files/figure-html/grass-30.png)![](palettes_files/figure-html/grass-31.png)![](palettes_files/figure-html/grass-32.png)![](palettes_files/figure-html/grass-33.png)![](palettes_files/figure-html/grass-34.png)![](palettes_files/figure-html/grass-35.png)![](palettes_files/figure-html/grass-36.png)![](palettes_files/figure-html/grass-37.png)![](palettes_files/figure-html/grass-38.png)![](palettes_files/figure-html/grass-39.png)![](palettes_files/figure-html/grass-40.png)![](palettes_files/figure-html/grass-41.png)![](palettes_files/figure-html/grass-42.png)![](palettes_files/figure-html/grass-43.png)![](palettes_files/figure-html/grass-44.png)![](palettes_files/figure-html/grass-45.png)![](palettes_files/figure-html/grass-46.png)![](palettes_files/figure-html/grass-47.png)![](palettes_files/figure-html/grass-48.png)![](palettes_files/figure-html/grass-49.png)![](palettes_files/figure-html/grass-50.png)![](palettes_files/figure-html/grass-51.png)

## `scale_fill_princess_*`

Check
[`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/reference/scale_princess.md)
for more info.

![](palettes_files/figure-html/princess-1.png)![](palettes_files/figure-html/princess-2.png)![](palettes_files/figure-html/princess-3.png)![](palettes_files/figure-html/princess-4.png)![](palettes_files/figure-html/princess-5.png)![](palettes_files/figure-html/princess-6.png)![](palettes_files/figure-html/princess-7.png)![](palettes_files/figure-html/princess-8.png)![](palettes_files/figure-html/princess-9.png)![](palettes_files/figure-html/princess-10.png)![](palettes_files/figure-html/princess-11.png)![](palettes_files/figure-html/princess-12.png)![](palettes_files/figure-html/princess-13.png)![](palettes_files/figure-html/princess-14.png)![](palettes_files/figure-html/princess-15.png)

## Session info

Details

    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.5.2 (2025-10-31 ucrt)
    #>  os       Windows Server 2022 x64 (build 26100)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language en
    #>  collate  English_United States.utf8
    #>  ctype    English_United States.utf8
    #>  tz       UTC
    #>  date     2025-11-15
    #>  pandoc   3.1.11 @ C:/HOSTED~1/windows/pandoc/31F387~1.11/x64/PANDOC~1.11/ (via rmarkdown)
    #>  quarto   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package      * version    date (UTC) lib source
    #>  bslib          0.9.0      2025-01-30 [1] RSPM
    #>  cachem         1.1.0      2024-05-16 [1] RSPM
    #>  class          7.3-23     2025-01-01 [3] CRAN (R 4.5.2)
    #>  classInt       0.4-11     2025-01-08 [1] RSPM
    #>  cli            3.6.5      2025-04-23 [1] RSPM
    #>  codetools      0.2-20     2024-03-31 [3] CRAN (R 4.5.2)
    #>  data.table     1.17.8     2025-07-10 [1] RSPM
    #>  DBI            1.2.3      2024-06-02 [1] RSPM
    #>  desc           1.4.3      2023-12-10 [1] RSPM
    #>  digest         0.6.38     2025-11-12 [1] RSPM
    #>  dplyr          1.1.4      2023-11-17 [1] RSPM
    #>  e1071          1.7-16     2024-09-16 [1] RSPM
    #>  evaluate       1.0.5      2025-08-27 [1] RSPM
    #>  farver         2.1.2      2024-05-13 [1] RSPM
    #>  fastmap        1.2.0      2024-05-15 [1] RSPM
    #>  fs             1.6.6      2025-04-12 [1] RSPM
    #>  generics       0.1.4      2025-05-09 [1] RSPM
    #>  ggplot2      * 4.0.0      2025-09-11 [1] RSPM
    #>  glue           1.8.0      2024-09-30 [1] RSPM
    #>  gtable         0.3.6      2024-10-25 [1] RSPM
    #>  htmltools      0.5.8.1    2024-04-04 [1] RSPM
    #>  htmlwidgets    1.6.4      2023-12-06 [1] RSPM
    #>  jquerylib      0.1.4      2021-04-26 [1] RSPM
    #>  jsonlite       2.0.0      2025-03-27 [1] RSPM
    #>  KernSmooth     2.23-26    2025-01-01 [3] CRAN (R 4.5.2)
    #>  knitr          1.50       2025-03-16 [1] RSPM
    #>  labeling       0.4.3      2023-08-29 [1] RSPM
    #>  lifecycle      1.0.4      2023-11-07 [1] RSPM
    #>  magrittr       2.0.4      2025-09-12 [1] RSPM
    #>  pillar         1.11.1     2025-09-17 [1] RSPM
    #>  pkgconfig      2.0.3      2019-09-22 [1] RSPM
    #>  pkgdown        2.2.0      2025-11-06 [1] any (@2.2.0)
    #>  proxy          0.4-27     2022-06-09 [1] RSPM
    #>  purrr          1.2.0      2025-11-04 [1] RSPM
    #>  R.cache        0.17.0     2025-05-02 [1] RSPM
    #>  R.methodsS3    1.8.2      2022-06-13 [1] RSPM
    #>  R.oo           1.27.1     2025-05-02 [1] RSPM
    #>  R.utils        2.13.0     2025-02-24 [1] RSPM
    #>  R6             2.6.1      2025-02-15 [1] RSPM
    #>  ragg           1.5.0      2025-09-02 [1] RSPM
    #>  RColorBrewer   1.1-3      2022-04-03 [1] RSPM
    #>  Rcpp           1.1.0      2025-07-02 [1] RSPM
    #>  rlang          1.1.6      2025-04-11 [1] RSPM
    #>  rmarkdown      2.30       2025-09-28 [1] RSPM
    #>  S7             0.2.0      2024-11-07 [1] RSPM
    #>  sass           0.4.10     2025-04-11 [1] RSPM
    #>  scales         1.4.0      2025-04-24 [1] RSPM
    #>  sessioninfo  * 1.2.3      2025-02-05 [1] RSPM
    #>  sf             1.0-22     2025-11-10 [1] RSPM
    #>  styler         1.11.0     2025-10-13 [1] RSPM
    #>  systemfonts    1.3.1      2025-10-01 [1] RSPM
    #>  terra          1.8-80     2025-11-05 [1] RSPM
    #>  textshaping    1.0.4      2025-10-10 [1] RSPM
    #>  tibble         3.3.0      2025-06-08 [1] RSPM
    #>  tidyr          1.3.1      2024-01-24 [1] RSPM
    #>  tidyselect     1.2.1      2024-03-11 [1] RSPM
    #>  tidyterra    * 0.7.2.9000 2025-11-15 [1] local
    #>  units          1.0-0      2025-10-09 [1] RSPM
    #>  vctrs          0.6.5      2023-12-01 [1] RSPM
    #>  withr          3.0.2      2024-10-28 [1] RSPM
    #>  xfun           0.54       2025-10-30 [1] RSPM
    #>  yaml           2.3.10     2024-07-26 [1] RSPM
    #> 
    #>  [1] D:/a/_temp/Library
    #>  [2] C:/R/site-library
    #>  [3] C:/R/library
    #>  * ── Packages attached to the search path.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
