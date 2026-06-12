# Package index

## **tibble** methods

Methods from [tibble](https://CRAN.R-project.org/package=tibble)
implemented for `Spat*` objects.

- [`as_tibble(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  [`as_tibble(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  :

  Coerce `SpatRaster` and `SpatVector` objects to tibbles

## **dplyr** methods

Methods from [dplyr](https://CRAN.R-project.org/package=dplyr)
implemented for `Spat*` objects.

### Rows

Verbs that mainly operate on rows.

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

- [`rows_insert(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_append(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_update(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_patch(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_upsert(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_delete(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  :

  Row operations for `SpatVector` objects

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

Verbs that mainly operate on columns.

- [`glimpse(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  [`glimpse(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  :

  Preview `Spat*` objects

- [`mutate(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md)
  [`mutate(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md)
  :

  Create, modify and delete cell values/layers/attributes of `Spat*`
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

Verbs that mainly operate on groups of rows.

- [`count(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  [`tally(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  [`add_count(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  :

  Count the observations in each `SpatVector` group

- [`group_by(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md)
  [`ungroup(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md)
  :

  Group a `SpatVector` by one or more variables

- [`reframe(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/reframe.SpatVector.md)
  :

  Reframe each group of a `SpatVector`

- [`rowwise(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md)
  :

  Group `SpatVector` objects by rows

- [`summarise(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  [`summarize(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  :

  Summarise each group of a `SpatVector` down to one geometry

### `SpatVector` and data frames

Verbs that mainly operate on pairs of `SpatVector` objects and data
frames.

- [`bind_spat_cols()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md)
  :

  Bind multiple `SpatVector`, `sf` and data frame objects by column

- [`bind_spat_rows()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md)
  :

  Bind multiple `SpatVector`, `sf/sfc` and data frame objects by row

- [`cross_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/cross_join.SpatVector.md)
  :

  Cross joins for `SpatVector` objects

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

- [`nest_join(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md)
  :

  Nest join `SpatVector` objects

- [`rows_insert(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_append(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_update(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_patch(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_upsert(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  [`rows_delete(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  :

  Row operations for `SpatVector` objects

## **tidyr** methods

Methods from [tidyr](https://CRAN.R-project.org/package=tidyr)
implemented for `Spat*` objects.

### Pivoting

Pivoting changes the representation of a `SpatVector` object without
changing its data.

- [`pivot_longer(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md)
  :

  Pivot `SpatVector` from wide to long

- [`pivot_wider(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md)
  :

  Pivot `SpatVector` from long to wide

### Rows

Tools for duplicating `SpatVector` features.

- [`uncount(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/uncount.SpatVector.md)
  :

  Duplicate `SpatVector` rows

### Missing values

Tools for converting between implicit (absent rows) and explicit (`NA`)
missing values and for handling explicit `NA` values.

- [`complete(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/complete.SpatVector.md)
  :

  Complete missing combinations in a `SpatVector`

- [`drop_na(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md)
  [`drop_na(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md)
  :

  Drop attributes of `Spat*` objects containing missing values

- [`expand(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/expand.SpatVector.md)
  :

  Expand `SpatVector` attribute combinations

- [`fill(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md)
  :

  Fill in missing values with previous or next value on a `SpatVector`

- [`replace_na(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md)
  [`replace_na(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md)
  :

  Replace `NA`s with specified values

### Character vectors

Tools for working with character layers and attributes.

- [`unite(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/unite.Spat.md)
  [`unite(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/unite.Spat.md)
  :

  Unite `Spat*` layers or attributes

### Nesting

Tools for storing `SpatVector` objects in nested list-columns.

- [`nest(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/nest.SpatVector.md)
  :

  Nest `SpatVector` rows

## **ggplot2** methods

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

## **ggplot2** helpers

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

  Plot `SpatRaster` objects

- [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  :

  Plot `SpatRaster` objects as images

- [`geom_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  [`geom_spatvector_label()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  [`geom_spatvector_text()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  [`stat_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  :

  Plot `SpatVector` objects

### Scales

Color scales, palettes and hypsometric tints for maps.

- [`scale_fill_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  [`scale_colour_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  [`get_coltab_pal()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  :

  Discrete scales based on `SpatRaster` color tables

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
  : Gradient scales for hypsometric and bathymetric tints

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

  Terrain color scales from grDevices

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

  Tidy `Spat*` objects for plotting

## Helpers

Additional helper functions provided by
[tidyterra](https://CRAN.R-project.org/package=tidyterra).

### Coercing objects

Convert `Spat*` objects to other object types or create `SpatRaster`
objects from tibbles.

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

  Coerce objects to `SpatVector`

- [`as_tibble(`*`<SpatRaster>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  [`as_tibble(`*`<SpatVector>`*`)`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  :

  Coerce `SpatRaster` and `SpatVector` objects to tibbles

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

  Tidy `Spat*` objects for plotting

### Other helpers

Utilities for CRS handling, package checks and `SpatRaster` comparison.

- [`compare_spatrasters()`](https://dieghernan.github.io/tidyterra/dev/reference/compare_spatrasters.md)
  :

  Compare attributes of two `SpatRaster` objects

- [`is_regular_grid()`](https://dieghernan.github.io/tidyterra/dev/reference/is_regular_grid.md)
  : Check whether x and y positions form a regular grid

- [`pull_crs()`](https://dieghernan.github.io/tidyterra/dev/reference/pull_crs.md)
  : Extract CRS in WKT format

## Built-in data

Example data and palette databases included with **tidyterra**.

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
