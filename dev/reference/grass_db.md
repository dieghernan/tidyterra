# GRASS color tables

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html)
including the color map of 51 gradient palettes. Some palettes also
include a definition of color limits that can be used with
[`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Format

A tibble of 2920 rows and 6 columns with the following fields:

- pal:

  Name of the palette.

- limit:

  (Optional) limit for each color.

- r:

  Value of the red channel (RGB color mode).

- g:

  Value of the green channel (RGB color mode).

- b:

  Value of the blue channel (RGB color mode).

- hex:

  Hex code of the color.

## Source

Derived from <https://github.com/OSGeo/grass/tree/main/lib/gis/colors>.
See also [r.color - GRASS GIS
Manual](https://grass.osgeo.org/grass-stable/manuals/r.colors.html).

## Details

Summary of palettes provided, description and recommended use:

|  |  |  |  |
|----|----|----|----|
| **palette** | **use** | **description** | **range** |
| `aspect` | General | Aspect-oriented gray colors |  |
| `aspectcolr` | General | Aspect-oriented rainbow colors | 0 to 360 |
| `bcyr` | General | Blue through cyan and yellow to red |  |
| `bgyr` | General | Blue through green and yellow to red |  |
| `blues` | General | White to blue |  |
| `byg` | General | Blue through yellow to green |  |
| `byr` | General | Blue through yellow to red |  |
| `celsius` | General | Blue to red for Celsius temperatures | -80 to 80 |
| `corine` | Land cover | EU Corine land cover colors | 111 to 995 |
| `curvature` | General | Terrain curvature colors | -0.1 to 0.1 |
| `differences` | General | Difference-oriented colors |  |
| `elevation` | Topography | Relative raster values mapped to elevation colors |  |
| `etopo2` | Topography | ETOPO2 worldwide bathymetry and topography colors | -11000 to 8850 |
| `evi` | Natural | Enhanced Vegetation Index colors | -1 to 1 |
| `fahrenheit` | Temperature | Blue to red for Fahrenheit temperatures | -112 to 176 |
| `forest_cover` | Natural | Percentage of forest cover | 0 to 1 |
| `gdd` | Natural | Accumulated growing degree days | 0 to 6000 |
| `grass` | General | Perceptually uniform GRASS GIS green |  |
| `greens` | General | White to green |  |
| `grey` | General | Grayscale |  |
| `gyr` | General | Green through yellow to red |  |
| `haxby` | Topography | Relative colors for bathymetry or topography |  |
| `inferno` | General | Inferno perceptually uniform sequential color table |  |
| `kelvin` | Temperature | Blue to red for temperatures in Kelvin | 193.15 to 353.15 |
| `magma` | General | Magma perceptually uniform sequential color table |  |
| `ndvi` | Natural | Normalized Difference Vegetation Index colors | -1 to 1 |
| `ndwi` | Natural | Normalized Difference Water Index colors | -200 to 200 |
| `nlcd` | Land cover | US National Land Cover Dataset colors | 0 to 95 |
| `oranges` | General | White to orange |  |
| `plasma` | General | Plasma perceptually uniform sequential color table |  |
| `population` | Human | Human population classification breaks | 0 to 1000000 |
| `population_dens` | Human | Human population density classification breaks | 0 to 1000 |
| `precipitation` | Climate | Precipitation color table, 0 to 2000 mm | 0 to 7000 |
| `precipitation_daily` | Climate | Daily precipitation color table, 0 to 1000 mm | 0 to 100 |
| `precipitation_monthly` | Climate | Monthly precipitation color table, 0 to 1000 mm | 0 to 1000 |
| `rainbow` | General | Rainbow color table |  |
| `ramp` | General | Color ramp |  |
| `reds` | General | White to red |  |
| `roygbiv` | General |  |  |
| `rstcurv` | General | Terrain curvature from r.resamp.rst | -0.1 to 0.1 |
| `ryb` | General | Red through yellow to blue |  |
| `ryg` | General | Red through yellow to green |  |
| `sepia` | General | Yellowish-brown to white |  |
| `slope` | General | `r.slope.aspect`-style slope colors for raster values from 0 to 90 | 0 to 90 |
| `soilmoisture` | Natural | Soil moisture color table, 0.0 to 1.0 | 0 to 1 |
| `srtm` | Topography | Shuttle Radar Topography Mission elevation colors | -11000 to 8850 |
| `srtm_plus` | Topography | Shuttle Radar Topography Mission elevation colors with seafloor colors | -11000 to 8850 |
| `terrain` | Topography | Global elevation color table from -11000 to +8850 m | -11000 to 8850 |
| `viridis` | General | Viridis perceptually uniform sequential color table |  |
| `water` | Natural | Water depth |  |
| `wave` | General | Color wave |  |

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::map.pal()`](https://rspatial.github.io/terra/reference/mappal.html).

## References

GRASS Development Team (2024). *Geographic Resources Analysis Support
System (GRASS) Software, Version 8.3.2*. Open Source Geospatial
Foundation, USA. <https://grass.osgeo.org>.

## See also

[`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md).

Other datasets:
[`cross_blended_hypsometric_tints_db`](https://dieghernan.github.io/tidyterra/dev/reference/cross_blended_hypsometric_tints_db.md),
[`hypsometric_tints_db`](https://dieghernan.github.io/tidyterra/dev/reference/hypsometric_tints_db.md),
[`princess_db`](https://dieghernan.github.io/tidyterra/dev/reference/princess_db.md),
[`volcano2`](https://dieghernan.github.io/tidyterra/dev/reference/volcano2.md)

## Examples

``` r
# \donttest{
data("grass_db")

grass_db
#> # A tibble: 2,920 × 6
#>    pal        limit     r     g     b hex    
#>    <chr>      <dbl> <dbl> <dbl> <dbl> <chr>  
#>  1 aspect        NA     0     0     0 #000000
#>  2 aspect        NA   255   255   255 #FFFFFF
#>  3 aspect        NA     0     0     0 #000000
#>  4 aspectcolr     0   255   255   255 #FFFFFF
#>  5 aspectcolr     1   255   255     0 #FFFF00
#>  6 aspectcolr    90     0   255     0 #00FF00
#>  7 aspectcolr   180     0   255   255 #00FFFF
#>  8 aspectcolr   270   255     0     0 #FF0000
#>  9 aspectcolr   360   255   255     0 #FFFF00
#> 10 bcyr          NA     0     0   255 #0000FF
#> # ℹ 2,910 more rows
# Select a palette

srtm_plus <- grass_db |>
  filter(pal == "srtm_plus")

f <- system.file("extdata/asia.tif", package = "tidyterra")
r <- terra::rast(f)

library(ggplot2)

p <- ggplot() +
  geom_spatraster(data = r) +
  labs(fill = "elevation")

p +
  scale_fill_gradientn(colors = srtm_plus$hex)


# Use with limits
p +
  scale_fill_gradientn(
    colors = srtm_plus$hex,
    values = scales::rescale(srtm_plus$limit),
    limit = range(srtm_plus$limit),
    na.value = "lightblue"
  )

# }
```
