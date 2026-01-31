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

![Terrain and wiki palettes in
tidyterra](palettes_files/figure-html/terr_wiki-1.png)![Terrain and wiki
palettes in tidyterra](palettes_files/figure-html/terr_wiki-2.png)

## `scale_fill_whitebox_*`

Check
[`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/reference/scale_whitebox.md)
for more info.

![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-1.png)![Whitebox palettes
in tidyterra](palettes_files/figure-html/whitebox-2.png)![Whitebox
palettes in
tidyterra](palettes_files/figure-html/whitebox-3.png)![Whitebox palettes
in tidyterra](palettes_files/figure-html/whitebox-4.png)![Whitebox
palettes in
tidyterra](palettes_files/figure-html/whitebox-5.png)![Whitebox palettes
in tidyterra](palettes_files/figure-html/whitebox-6.png)![Whitebox
palettes in
tidyterra](palettes_files/figure-html/whitebox-7.png)![Whitebox palettes
in tidyterra](palettes_files/figure-html/whitebox-8.png)![Whitebox
palettes in
tidyterra](palettes_files/figure-html/whitebox-9.png)![Whitebox palettes
in tidyterra](palettes_files/figure-html/whitebox-10.png)![Whitebox
palettes in tidyterra](palettes_files/figure-html/whitebox-11.png)

## `scale_fill_hypso_*`

Check
[`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/reference/scale_hypso.md)
for more info.

![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-1.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-2.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-3.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-4.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-5.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-6.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-7.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-8.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-9.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-10.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-11.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-12.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-13.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-14.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-15.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-16.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-17.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-18.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-19.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-20.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-21.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-22.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-23.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-24.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-25.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-26.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-27.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-28.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-29.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-30.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-31.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-32.png)![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-33.png)

## `scale_fill_cross_blended_*`

Check
[`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/reference/scale_cross_blended.md)
for more info.

![Cross-blended palettes in
tidyterra](palettes_files/figure-html/cross-1.png)![Cross-blended
palettes in
tidyterra](palettes_files/figure-html/cross-2.png)![Cross-blended
palettes in
tidyterra](palettes_files/figure-html/cross-3.png)![Cross-blended
palettes in tidyterra](palettes_files/figure-html/cross-4.png)

## `scale_fill_grass_*`

Check
[`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/reference/scale_grass.md)
for more info. Plots produced using `use_grass_range = FALSE`.

![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-1.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-2.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-3.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-4.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-5.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-6.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-7.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-8.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-9.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-10.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-11.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-12.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-13.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-14.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-15.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-16.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-17.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-18.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-19.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-20.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-21.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-22.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-23.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-24.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-25.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-26.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-27.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-28.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-29.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-30.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-31.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-32.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-33.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-34.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-35.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-36.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-37.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-38.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-39.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-40.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-41.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-42.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-43.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-44.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-45.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-46.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-47.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-48.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-49.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-50.png)![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-51.png)

## `scale_fill_princess_*`

Check
[`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/reference/scale_princess.md)
for more info.

![Princess palettes in
tidyterra](palettes_files/figure-html/princess-1.png)![Princess palettes
in tidyterra](palettes_files/figure-html/princess-2.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-3.png)![Princess palettes
in tidyterra](palettes_files/figure-html/princess-4.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-5.png)![Princess palettes
in tidyterra](palettes_files/figure-html/princess-6.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-7.png)![Princess palettes
in tidyterra](palettes_files/figure-html/princess-8.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-9.png)![Princess palettes
in tidyterra](palettes_files/figure-html/princess-10.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-11.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-12.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-13.png)![Princess
palettes in
tidyterra](palettes_files/figure-html/princess-14.png)![Princess
palettes in tidyterra](palettes_files/figure-html/princess-15.png)

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
    #>  date     2026-01-31
    #>  pandoc   3.1.11 @ C:/HOSTED~1/windows/pandoc/31F387~1.11/x64/PANDOC~1.11/ (via rmarkdown)
    #>  quarto   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package      * version    date (UTC) lib source
    #>  bslib          0.10.0     2026-01-26 [1] RSPM
    #>  cachem         1.1.0      2024-05-16 [1] RSPM
    #>  class          7.3-23     2025-01-01 [3] CRAN (R 4.5.2)
    #>  classInt       0.4-11     2025-01-08 [1] RSPM
    #>  cli            3.6.5      2025-04-23 [1] RSPM
    #>  codetools      0.2-20     2024-03-31 [3] CRAN (R 4.5.2)
    #>  data.table     1.18.2.1   2026-01-27 [1] RSPM
    #>  DBI            1.2.3      2024-06-02 [1] RSPM
    #>  desc           1.4.3      2023-12-10 [1] RSPM
    #>  digest         0.6.39     2025-11-19 [1] RSPM
    #>  dplyr          1.1.4      2023-11-17 [1] RSPM
    #>  e1071          1.7-17     2025-12-18 [1] RSPM
    #>  evaluate       1.0.5      2025-08-27 [1] RSPM
    #>  farver         2.1.2      2024-05-13 [1] RSPM
    #>  fastmap        1.2.0      2024-05-15 [1] RSPM
    #>  fs             1.6.6      2025-04-12 [1] RSPM
    #>  generics       0.1.4      2025-05-09 [1] RSPM
    #>  ggplot2      * 4.0.1      2025-11-14 [1] RSPM
    #>  glue           1.8.0      2024-09-30 [1] RSPM
    #>  gtable         0.3.6      2024-10-25 [1] RSPM
    #>  htmltools      0.5.9      2025-12-04 [1] RSPM
    #>  htmlwidgets    1.6.4      2023-12-06 [1] RSPM
    #>  jquerylib      0.1.4      2021-04-26 [1] RSPM
    #>  jsonlite       2.0.0      2025-03-27 [1] RSPM
    #>  KernSmooth     2.23-26    2025-01-01 [3] CRAN (R 4.5.2)
    #>  knitr          1.51       2025-12-20 [1] RSPM
    #>  labeling       0.4.3      2023-08-29 [1] RSPM
    #>  lifecycle      1.0.5      2026-01-08 [1] RSPM
    #>  magrittr       2.0.4      2025-09-12 [1] RSPM
    #>  otel           0.2.0      2025-08-29 [1] RSPM
    #>  pillar         1.11.1     2025-09-17 [1] RSPM
    #>  pkgconfig      2.0.3      2019-09-22 [1] RSPM
    #>  pkgdown        2.2.0      2025-11-06 [1] RSPM
    #>  proxy          0.4-29     2025-12-29 [1] RSPM
    #>  purrr          1.2.1      2026-01-09 [1] RSPM
    #>  R.cache        0.17.0     2025-05-02 [1] RSPM
    #>  R.methodsS3    1.8.2      2022-06-13 [1] RSPM
    #>  R.oo           1.27.1     2025-05-02 [1] RSPM
    #>  R.utils        2.13.0     2025-02-24 [1] RSPM
    #>  R6             2.6.1      2025-02-15 [1] RSPM
    #>  ragg           1.5.0      2025-09-02 [1] RSPM
    #>  RColorBrewer   1.1-3      2022-04-03 [1] RSPM
    #>  Rcpp           1.1.1      2026-01-10 [1] RSPM
    #>  rlang          1.1.7      2026-01-09 [1] RSPM
    #>  rmarkdown      2.30       2025-09-28 [1] RSPM
    #>  S7             0.2.1      2025-11-14 [1] RSPM
    #>  sass           0.4.10     2025-04-11 [1] RSPM
    #>  scales         1.4.0      2025-04-24 [1] RSPM
    #>  sessioninfo  * 1.2.3      2025-02-05 [1] any (@1.2.3)
    #>  sf             1.0-24     2026-01-13 [1] RSPM
    #>  styler         1.11.0     2025-10-13 [1] RSPM
    #>  systemfonts    1.3.1      2025-10-01 [1] RSPM
    #>  terra          1.8-93     2026-01-12 [1] RSPM
    #>  textshaping    1.0.4      2025-10-10 [1] RSPM
    #>  tibble         3.3.1      2026-01-11 [1] RSPM
    #>  tidyr          1.3.2      2025-12-19 [1] RSPM
    #>  tidyselect     1.2.1      2024-03-11 [1] RSPM
    #>  tidyterra    * 1.0.0.9000 2026-01-31 [1] local
    #>  units          1.0-0      2025-10-09 [1] RSPM
    #>  vctrs          0.7.1      2026-01-23 [1] RSPM
    #>  withr          3.0.2      2024-10-28 [1] RSPM
    #>  xfun           0.56       2026-01-18 [1] RSPM
    #>  yaml           2.3.12     2025-12-10 [1] RSPM
    #> 
    #>  [1] D:/a/_temp/Library
    #>  [2] C:/R/site-library
    #>  [3] C:/R/library
    #>  * ── Packages attached to the search path.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
