# Package index

## **tibble** methods

Methods from [tibble](https://CRAN.R-project.org/package=tibble)
implemented for `Spat*` objects.

- [`as_tibble(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  [`as_tibble(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  :

  Coerce a `SpatVector` or `SpatRaster` object to data frames

## dplyr methods

Methods from [dplyr](https://CRAN.R-project.org/package=dplyr)
implemented for `Spat*` objects.

### Rows

Verbs that principally operate on rows.

- [`arrange(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md)
  :

  Order a `SpatVector` using column values

- [`distinct(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md)
  :

  Keep distinct/unique rows and geometries of `SpatVector` objects

- [`filter(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md)
  [`filter(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md)
  [`filter_out(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md)
  :

  Subset cells/geometries of `Spat*` objects

- [`slice(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_head(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_head(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_tail(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_tail(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_min(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_min(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_max(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_max(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_sample(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_sample(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_rows()`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_cols()`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  [`slice_colrows()`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  : Subset cells/rows/columns/geometries using their positions

### Columns

Verbs that principally operate on columns.

- [`glimpse(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  [`glimpse(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  :

  Get a nice glimpse of your `Spat*` objects

- [`mutate(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md)
  [`mutate(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md)
  :

  Create, modify, and delete cell values/layers/attributes of `Spat*`
  objects

- [`pull(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md)
  [`pull(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md)
  : Extract a single layer/attribute

- [`relocate(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md)
  [`relocate(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md)
  : Change layer/attribute order

- [`rename(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md)
  [`rename_with(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md)
  [`rename(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md)
  [`rename_with(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md)
  : Rename layers/attributes

- [`select(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md)
  [`select(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md)
  :

  Subset layers/attributes of `Spat*` objects

### Groups

Verbs that principally operate on groups of rows.

- [`count(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  [`tally(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  [`add_count(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  :

  Count the observations in each `SpatVector` group

- [`group_by(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md)
  [`ungroup(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md)
  :

  Group a `SpatVector` by one or more variables

- [`rowwise(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md)
  :

  Group `SpatVector` objects by rows

- [`summarise(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  [`summarize(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  :

  Summarise each group of a `SpatVector` down to one geometry

### `SpatVector` and Data frames

Verbs that principally operate on pairs of `Spat*` and data frames.

- [`bind_spat_cols()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md)
  :

  Bind multiple `SpatVector` `sf` and data frames objects by column

- [`bind_spat_rows()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md)
  :

  Bind multiple `SpatVector`, `sf/sfc` and data frames objects by row

- [`semi_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md)
  [`anti_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md)
  :

  Filtering joins for `SpatVector` objects

- [`inner_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)
  [`left_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)
  [`right_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)
  [`full_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)
  :

  Mutating joins for `SpatVector` objects

## tidyr methods

Methods from [tidyr](https://CRAN.R-project.org/package=tidyr)
implemented for `Spat*` objects.

### Pivoting

Pivoting changes the representation of a `SpatVector` object, without
changing the data inside of it.

- [`pivot_longer(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md)
  :

  Pivot `SpatVector` from wide to long

- [`pivot_wider(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md)
  :

  Pivot `SpatVector` from long to wide

### Missing values

Tools for converting between implicit (absent rows) and explicit (`NA`)
missing values, handling explicit `NA`s.

- [`drop_na(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md)
  [`drop_na(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md)
  :

  Drop attributes of `Spat*` objects containing missing values

- [`fill(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md)
  :

  Fill in missing values with previous or next value on a `SpatVector`

- [`replace_na(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md)
  [`replace_na(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md)
  :

  Replace `NA`s with specified values

## ggplot2 methods

Methods from [ggplot2](https://CRAN.R-project.org/package=ggplot2)
implemented for `Spat*` objects.

- [`autoplot(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  [`autoplot(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  [`autoplot(`*`<SpatGraticule>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  [`autoplot(`*`<SpatExtent>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  :

  Create a complete ggplot for `Spat*` objects

- [`fortify(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  [`fortify(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  [`fortify(`*`<SpatGraticule>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  [`fortify(`*`<SpatExtent>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  :

  Fortify `Spat*` Objects

## ggplot2 utils

### Geoms

Create [ggplot2](https://CRAN.R-project.org/package=ggplot2) layers for
`Spat*` objects.

- [`geom_spatraster_contour()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  [`geom_spatraster_contour_text()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  [`geom_spatraster_contour_filled()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  :

  Plot `SpatRaster` contours

- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  [`stat_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  :

  Visualise `SpatRaster` objects

- [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  :

  Visualise `SpatRaster` objects as images

- [`geom_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  [`geom_spatvector_label()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  [`geom_spatvector_text()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  [`stat_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  :

  Visualise `SpatVector` objects

### Scales

Gradient colour schemes, palettes and hypsometric tints.

- [`scale_fill_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  [`scale_colour_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  [`get_coltab_pal()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  :

  Discrete scales based in the color table of a `SpatRaster`

- [`scale_fill_cross_blended_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_colour_cross_blended_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_colour_cross_blended_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_fill_cross_blended_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_colour_cross_blended_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`cross_blended.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_fill_cross_blended_tint_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_colour_cross_blended_tint_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_fill_cross_blended_tint_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_colour_cross_blended_tint_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_fill_cross_blended_tint_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`scale_colour_cross_blended_tint_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  [`cross_blended.colors2()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
  : Cross blended hypsometric tints scales

- [`scale_fill_grass_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  [`scale_colour_grass_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  [`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  [`scale_colour_grass_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  [`scale_fill_grass_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  [`scale_colour_grass_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  [`grass.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  : GRASS scales

- [`scale_fill_hypso_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_colour_hypso_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_colour_hypso_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_fill_hypso_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_colour_hypso_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`hypso.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_fill_hypso_tint_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_colour_hypso_tint_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_fill_hypso_tint_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_colour_hypso_tint_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_fill_hypso_tint_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`scale_colour_hypso_tint_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  [`hypso.colors2()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  : Gradient scales for representing hypsometry and bathymetry

- [`scale_fill_princess_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  [`scale_colour_princess_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  [`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  [`scale_colour_princess_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  [`scale_fill_princess_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  [`scale_colour_princess_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  [`princess.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  : Gradient scales from princess color schemes

- [`scale_fill_terrain_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md)
  [`scale_colour_terrain_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md)
  [`scale_fill_terrain_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md)
  [`scale_colour_terrain_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md)
  [`scale_fill_terrain_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md)
  [`scale_colour_terrain_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md)
  :

  Terrain colour scales from grDevices

- [`scale_fill_whitebox_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  [`scale_colour_whitebox_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  [`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  [`scale_colour_whitebox_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  [`scale_fill_whitebox_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  [`scale_colour_whitebox_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  [`whitebox.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  :

  Gradient scales from **WhiteboxTools** color schemes

- [`scale_fill_wiki_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  [`scale_colour_wiki_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  [`scale_fill_wiki_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  [`scale_colour_wiki_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  [`scale_fill_wiki_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  [`scale_colour_wiki_b()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  [`wiki.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  :

  Gradient scales from **Wikipedia** color schemes

## **generics** methods

Methods from [generics](https://CRAN.R-project.org/package=generics)
implemented for `Spat*` objects.

- [`glance(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/glance.Spat.md)
  [`glance(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/glance.Spat.md)
  :

  Glance at an `Spat*` object

- [`required_pkgs(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/required_pkgs.Spat.md)
  [`required_pkgs(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/required_pkgs.Spat.md)
  [`required_pkgs(`*`<SpatGraticule>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/required_pkgs.Spat.md)
  [`required_pkgs(`*`<SpatExtent>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/required_pkgs.Spat.md)
  :

  Determine packages required by `Spat*` objects

- [`tidy(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  [`tidy(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  [`tidy(`*`<SpatGraticule>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  [`tidy(`*`<SpatExtent>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  :

  Turn `Spat*` object into a tidy tibble

## Helpers

Additional set of functions provided by
[tidyterra](https://CRAN.R-project.org/package=tidyterra).

### Coercing objects

Convert Spat\* to other type of objects or create `SpatRasters` from
tibbles.

- [`as_coordinates()`](https://dieghernan.github.io/tidyterra/dev/reference/as_coordinates.md)
  :

  Get cell number, row and column from a `SpatRaster`

- [`as_sf()`](https://dieghernan.github.io/tidyterra/dev/reference/as_sf.md)
  :

  Coerce a `SpatVector` to a
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object

- [`as_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatraster.md)
  :

  Coerce a data frame to `SpatRaster`

- [`as_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatvector.md)
  :

  Method for coercing objects to `SpatVector`

- [`as_tibble(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  [`as_tibble(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  :

  Coerce a `SpatVector` or `SpatRaster` object to data frames

- [`fortify(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  [`fortify(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  [`fortify(`*`<SpatGraticule>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  [`fortify(`*`<SpatExtent>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  :

  Fortify `Spat*` Objects

- [`tidy(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  [`tidy(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  [`tidy(`*`<SpatGraticule>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  [`tidy(`*`<SpatExtent>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  :

  Turn `Spat*` object into a tidy tibble

### Other helpers

- [`compare_spatrasters()`](https://dieghernan.github.io/tidyterra/dev/reference/compare_spatrasters.md)
  :

  Compare attributes of two `SpatRaster` objects

- [`is_regular_grid()`](https://dieghernan.github.io/tidyterra/dev/reference/is_regular_grid.md)
  : Check if x and y positions conforms a regular grid

- [`pull_crs()`](https://dieghernan.github.io/tidyterra/dev/reference/pull_crs.md)
  : Extract CRS on WKT format

## Built in data

- [`cross_blended_hypsometric_tints_db`](https://dieghernan.github.io/tidyterra/dev/reference/cross_blended_hypsometric_tints_db.md)
  : Cross-blended hypsometric tints
- [`grass_db`](https://dieghernan.github.io/tidyterra/dev/reference/grass_db.md)
  : GRASS color tables
- [`hypsometric_tints_db`](https://dieghernan.github.io/tidyterra/dev/reference/hypsometric_tints_db.md)
  : Hypsometric palettes database
- [`princess_db`](https://dieghernan.github.io/tidyterra/dev/reference/princess_db.md)
  : Princess palettes database
- [`volcano2`](https://dieghernan.github.io/tidyterra/dev/reference/volcano2.md)
  : Updated topographic information on Auckland's Maungawhau volcano

## About the package

- [`tidyterra`](https://dieghernan.github.io/tidyterra/dev/reference/tidyterra-package.md)
  [`tidyterra-package`](https://dieghernan.github.io/tidyterra/dev/reference/tidyterra-package.md)
  : tidyterra: 'tidyverse' Methods and 'ggplot2' Helpers for 'terra'
  Objects
