# Extract CRS on WKT format

Extract the WKT version of the CRS associated to a string, number of
sf/Spat\* object.

The [Well-known text
(WKT)](https://en.wikipedia.org/wiki/Well-known_text_representation_of_coordinate_reference_systems)
representation of coordinate reference systems (CRS) is a character
string that identifies precisely the arguments of each CRS. This is the
current standard used on [sf](https://CRAN.R-project.org/package=sf) and
[terra](https://CRAN.R-project.org/package=terra) packages.

## Usage

``` r
pull_crs(.data, ...)
```

## Arguments

- .data:

  Input potentially including or representing a CRS. It could be a
  `sf/sfc` object, a `SpatRaster/SpatVector` object, a `crs` object from
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html),
  a character (for example a [proj4
  string](https://proj.org/en/9.3/operations/projections/index.html)) or
  a integer (representing an [EPSG](https://epsg.io/) code).

- ...:

  ignored

## Value

A WKT representation of the corresponding CRS.

## Details

Although the WKT representation is the same,
[sf](https://CRAN.R-project.org/package=sf) and
[terra](https://CRAN.R-project.org/package=terra) API slightly differs.
For example, [sf](https://CRAN.R-project.org/package=sf) can do:

`sf::st_transform(x, 25830)`

While [sf](https://CRAN.R-project.org/package=sf) equivalent is:

`terra::project(bb, "epsg:25830")`

Knowing the WKT would help to smooth workflows when working with
different packages and object types.

## Internals

This is a thin wrapper of
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
and
[`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

## See also

[`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html),
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
for knowing how these packages handle CRS definitions.

Other helpers:
[`compare_spatrasters()`](https://dieghernan.github.io/tidyterra/dev/reference/compare_spatrasters.md),
[`is_grouped_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/is_grouped_spatvector.md),
[`is_regular_grid()`](https://dieghernan.github.io/tidyterra/dev/reference/is_regular_grid.md)

## Examples

``` r
# sf objects

sfobj <- sf::st_as_sfc("MULTIPOINT ((0 0), (1 1))", crs = 4326)

fromsf1 <- pull_crs(sfobj)
fromsf2 <- pull_crs(sf::st_crs(sfobj))

# terra

v <- terra::vect(sfobj)
r <- terra::rast(v)

fromterra1 <- pull_crs(v)
fromterra2 <- pull_crs(r)

# integers
fromint <- pull_crs(4326)

# Characters
fromchar <- pull_crs("epsg:4326")


all(
  fromsf1 == fromsf2,
  fromsf2 == fromterra1,
  fromterra1 == fromterra2,
  fromterra2 == fromint,
  fromint == fromchar
)
#> [1] TRUE

cat(fromsf1)
#> GEOGCRS["WGS 84",
#>     ENSEMBLE["World Geodetic System 1984 ensemble",
#>         MEMBER["World Geodetic System 1984 (Transit)"],
#>         MEMBER["World Geodetic System 1984 (G730)"],
#>         MEMBER["World Geodetic System 1984 (G873)"],
#>         MEMBER["World Geodetic System 1984 (G1150)"],
#>         MEMBER["World Geodetic System 1984 (G1674)"],
#>         MEMBER["World Geodetic System 1984 (G1762)"],
#>         MEMBER["World Geodetic System 1984 (G2139)"],
#>         MEMBER["World Geodetic System 1984 (G2296)"],
#>         ELLIPSOID["WGS 84",6378137,298.257223563,
#>             LENGTHUNIT["metre",1]],
#>         ENSEMBLEACCURACY[2.0]],
#>     PRIMEM["Greenwich",0,
#>         ANGLEUNIT["degree",0.0174532925199433]],
#>     CS[ellipsoidal,2],
#>         AXIS["geodetic latitude (Lat)",north,
#>             ORDER[1],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         AXIS["geodetic longitude (Lon)",east,
#>             ORDER[2],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>     USAGE[
#>         SCOPE["Horizontal component of 3D system."],
#>         AREA["World."],
#>         BBOX[-90,-180,90,180]],
#>     ID["EPSG",4326]]
```
