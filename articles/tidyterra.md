# Using the tidyverse with terra objects: the tidyterra package

[![DOI](https://joss.theoj.org/papers/10.21105/joss.05751/status.svg)](https://doi.org/10.21105/joss.05751)

## Summary

**tidyterra** is an **R** ([R Core Team 2023](#ref-r-project)) package
that allows manipulation of spatial data objects as provided by the
**terra** package ([Hijmans 2023](#ref-R-terra)), using the verbs of the
packages included in the **tidyverse** ([Wickham et al.
2019](#ref-R-tidyverse)), such as **dplyr** ([Wickham et al.
2023](#ref-R-dplyr)), **tidyr** ([Wickham, Vaughan, and Girlich
2023](#ref-R-tidyr)), or **tibble** ([Müller and Wickham
2023](#ref-R-tibble)). This addition enables users that are already
familiar with the **tidyverse** to approach spatial data manipulation
and analysis more easily and much faster.

Furthermore, **tidyterra** extends the functionality of the **ggplot2**
package ([Wickham 2016](#ref-R-ggplot2)) by providing additional `geoms`
and `stats` [¹](#fn1) like
[`geom_spatraster()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster.md)
and
[`geom_spatvector()`](https://dieghernan.github.io/tidyterra/reference/ggspatvector.md),
as well as carefully chosen scales and color palettes specifically
designed for map production.

**tidyterra** can manipulate the following classes of **terra** objects:

1.  `SpatVector` objects, which represent vector data such as points,
    lines, or polygon geometries.

2.  `SpatRaster` objects, which represent raster data in the form of a
    grid consisting of equally sized rectangles. Each rectangle can
    contain one or more values.

The first stable version of **tidyterra** was included on CRAN on April
24, 2022, and has been actively used by other packages (such as
**ebvcuve** ([Quoss et al. 2021](#ref-R-ebvcube)), **biomod2**
([Thuiller et al. 2023](#ref-R-biomod2)), **inlabru** ([Bachl et al.
2019](#ref-R-inlabru)), **RCzechia** ([Lacko 2023](#ref-R-rczechia)) and
**sparrpowR** ([Buller et al. 2021](#ref-R-sparrpowr))) and cited in
academic research and publications (Bahlburg et al.
([2023](#ref-bahlburg2023)), Moraga ([2024](#ref-moraga2023)), Leonardi,
Colucci, and Manica ([2023](#ref-Leonardi2023)), Meister et al.
([2023](#ref-meister2023))) ever since.

## Statement of need

The [**tidyverse**](https://tidyverse.org/) is a compilation of **R**
packages that share an underlying design philosophy, grammar, and data
structures. The packages within the tidyverse are widely used by **R**
users for tidying, transforming, and visualizing data.

The **tidyverse** is designed to work with tidy data (*“every column is
a variable, every row is an observation, every cell is a single
value”*), represented in the form of data frames or **tibbles**.
However, it is possible to extend the functionality of **tidyverse**
packages to work with new **R** object classes by registering the
corresponding S3 methods ([Wickham 2019](#ref-wickham_s32019)). This
means that
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
can be adapted to work with any object of class `foo` by creating the
corresponding S3 method `mutate.foo()`.

While other popular packages designed for spatial data handling, such as
**sf** ([Pebesma 2018](#ref-R-sf)) or **stars** ([Pebesma and Bivand
2023](#ref-R-stars)), already provide integration with the **tidyverse**
as part of their infrastructure, **terra** objects lack this integration
natively. Although **terra** offers a wide set of functions for
transforming and visualizing `SpatRaster` and `SpatVector` objects, some
users who are not familiar with this package would need to make an
additional effort to learn that syntax. This may imply an additional
challenge during their initial steps in the field of spatial analysis.

The **tidyterra** package was developed to address this integration gap.
By providing the corresponding S3 methods, data analysts can apply the
same syntax and functions they are already familiar with for rectangular
data to the objects provided by **terra**. This enables users who are
not familiar with spatial data analysis to approach this area more
easily.

In addition, **tidyterra** also offers functions for plotting **terra**
objects using the **ggplot2** syntax. Although packages like
**rasterVis** ([Perpiñán and Hijmans 2023](#ref-R-rastervis)) and
**ggspatial** ([Dunnington 2023](#ref-R-ggspatial)) already allow the
representation of `SpatRaster` objects via **ggplot2**, **tidyterra**
functions provide additional support for advanced mapping. This support
includes the integration of faceted maps, contours, and the automatic
conversion of spatial layers to the same CRS[²](#fn2) via
[`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).
Furthermore, **tidyterra** also provides support for `SpatVector`
objects, similar to the native support of **sf** objects in the
**ggplot2** package.

Lastly, **tidyterra** provides a collection of color palettes
specifically designed for representing spatial phenomena ([Lindsay
2018](#ref-whitebox)). Additionally, it implements the cross-blended
hypsometric tints described by Patterson and Jenny
([2011](#ref-Patterson_Jenny_2011)).

## A note on performance

The development philosophy of **tidyterra** consists on adapting
**terra** objects to data frame-like structures by performing different
data transformations, that ultimately may impact in the performance of
the package.

When manipulating large raster files (i.e. more than 10.000.000 cells),
it is recommended to use the native **terra** syntax, that is
specifically designed for handling this type of files. In the case of
plotting, the default behavior of the geoms provided is to resample
`SpatRaster` that presents more than 500.000 cells to speed up the
process (as the
[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)
does), however this upper limit can be modified using the `maxcell`
parameter of the geom function.

Note also that when possible, the help page of each function of
**tidyterra** references its equivalent in **terra**.

## Example of use

**tidyterra** is available on
[**CRAN**](https://CRAN.R-project.org/package=tidyterra), so it can be
easily installed using the following commands in **R**:

``` r
install.packages("tidyterra")
```

The latest developing version is hosted in
[GitHub](https://github.com/dieghernan/tidyterra) and can be installed
using the following command in **R**:

``` r
remotes::install_github("dieghernan/tidyterra")
```

The following example demonstrates how to manipulate a `SpatRaster`
object using the **dplyr** syntax. Additionally, it illustrates how to
seamlessly plot a `SpatRaster` object with **ggplot2** using the
[`geom_spatraster()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster.md)
function:

``` r
library(tidyterra)
library(tidyverse) # Load all the packages of tidyverse at once
library(scales) # Additional library for labels

# Temperatures in Castille and Leon (selected months)
rastertemp <- terra::rast(system.file("extdata/cyl_temp.tif",
  package = "tidyterra"
))
#> Error:
#> ! [rast] filename is empty. Provide a valid filename

# Rename with the tidyverse
rastertemp <- rastertemp |>
  rename(April = tavg_04, May = tavg_05, June = tavg_06)
#> Error:
#> ! object 'rastertemp' not found


# Plot with facets
ggplot() +
  geom_spatraster(data = rastertemp) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = label_number(suffix = "º"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = "Average temperature in Castille and Leon (Spain)",
    subtitle = "Months of April, May and June"
  )
#> Error:
#> ! object 'rastertemp' not found
```

In the following example we combine a common **dplyr** workflow
([`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) +
[`select()`](https://dplyr.tidyverse.org/reference/select.html)) and we
plot the result. In this case the plot is a contour plot of the original
`SpatRaster` using
[`geom_spatraster_contour_filled()`](https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.md)and
it also includes an overlay of a `SpatVector` for reference:

``` r
# Compute the variation between April and June and apply a different palette
incr_temp <- rastertemp |>
  mutate(var = June - April) |>
  select(Variation = var)
#> Error:
#> ! object 'rastertemp' not found

# Overlay an SpatVector
cyl_vect <- terra::vect(system.file("extdata/cyl.gpkg",
  package = "tidyterra"
))
#> Error:
#> ! [vect] file does not exist:

# Contour map with overlay
ggplot() +
  geom_spatraster_contour_filled(data = incr_temp) +
  geom_spatvector(data = cyl_vect, fill = NA) +
  scale_fill_whitebox_d(palette = "bl_yl_rd") +
  theme_grey() +
  labs(
    fill = "º Celsius",
    title = "Variation of temperature in Castille and Leon (Spain)",
    subtitle = "Difference between April and June"
  )
#> Error:
#> ! object 'incr_temp' not found
```

## Additional materials

The package includes extensive documentation available online at
<https://dieghernan.github.io/tidyterra/> including:

- Details on each function, including (if possible) the equivalent
  **terra** function, in case users prefer to include those on their
  workflows.
- Working examples on the use of the functions and creation of plots.
- Additional articles and vignettes, as well as a complete demo of the
  different color palettes included on the package (see
  [Palettes](https://dieghernan.github.io/tidyterra/articles/palettes.html)).

## Acknowledgements

I would like to thank [Robert J. Hijmans](https://github.com/rhijmans)
for his advice and support in adapting some of the methods, as well as
for the suggestions that helped us improve the functionalities of the
package. I am also thankful to [Dewey
Dunnington](https://dewey.dunnington.ca/), Brent Thorne and the rest of
contributors of the **ggspatial** package, which served as a key
reference during the initial stages of the development of **tidyterra**.

**tidyterra** also incorporates some pieces of code adapted from
**ggplot2** for computing contours, which relies on the package
**isoband** ([Wickham, Wilke, and Pedersen 2022](#ref-R-isoband))
developed by [Claus O. Wilke](https://clauswilke.com/).

## References

Bachl, Fabian E., Finn Lindgren, David L. Borchers, and Janine B.
Illian. 2019. “inlabru: An R Package for Bayesian Spatial Modelling from
Ecological Survey Data.” *Methods in Ecology and Evolution* 10: 760–66.
<https://doi.org/10.1111/2041-210X.13168>.

Bahlburg, Dominik, Sally E. Thorpe, Bettina Meyer, Uta Berger, and
Eugene J. Murphy. 2023. “An Intercomparison of Models Predicting Growth
of Antarctic Krill (Euphausia superba): The Importance of Recognizing
Model Specificity.” *PLOS ONE* 18 (7): 1–29.
<https://doi.org/10.1371/journal.pone.0286036>.

Buller, I. D., D. W. Brown, T. A. Myers, R. R. Jones, and M. J.
Machiela. 2021. “sparrpowR: A Flexible R Package to Estimate Statistical
Power to Identify Spatial Clustering of Two Groups and Its Application.”
*International Journal of Health Geographics* 20 (1): 1–7.
<https://doi.org/10.1186/s12942-021-00267-z>.

Dunnington, Dewey. 2023. *ggspatial: Spatial Data Framework for
ggplot2*. <https://CRAN.R-project.org/package=ggspatial>.

Hijmans, Robert J. 2023. *terra: Spatial Data Analysis*.
<https://rspatial.org/>.

Lacko, Jindra. 2023. “RCzechia: Spatial Objects of the Czech Republic.”
*Journal of Open Source Software* 8 (83).
<https://doi.org/10.21105/joss.05082>.

Leonardi, Michela, Margherita Colucci, and Andrea Manica. 2023.
“tidysdm: Leveraging the Flexibility of tidymodels for Species
Distribution Modelling in R.” *bioRxiv*.
<https://doi.org/10.1101/2023.07.24.550358>.

Lindsay, John. 2018. “Whitebox-Tools.” GitHub repository.
<https://github.com/jblindsay/whitebox-tools>.

Meister, Nadja, Tom J. Langbehn, Øystein Varpe, and Christian Jørgensen.
2023. “Blue Mussels in Western Norway Have Vanished Where in Reach of
Crawling Predators.” *Marine Ecology Progress Series* 721 (October):
85–101. <https://doi.org/10.3354/meps14416>.

Moraga, Paula. 2024. *Spatial Statistics for Data Science: Theory and
Practice with R*. First edition. Boca Raton, FL: Chapman and Hall/CRC.
<https://www.paulamoraga.com/book-spatial/>.

Müller, Kirill, and Hadley Wickham. 2023. “tibble: Simple Data Frames.”
<https://tibble.tidyverse.org/>.

Patterson, Tom, and Bernhard Jenny. 2011. “The Development and Rationale
of Cross-Blended Hypsometric Tints.” *Cartographic Perspectives*, no. 69
(June): 31–46. <https://doi.org/10.14714/CP69.20>.

Pebesma, Edzer. 2018. “Simple Features for R: Standardized Support for
Spatial Vector Data.” *The R Journal* 10 (1): 439–46.
<https://doi.org/10.32614/RJ-2018-009>.

Pebesma, Edzer, and Roger Bivand. 2023. *Spatial Data Science: With
applications in R*. London: Chapman and Hall/CRC.
<https://doi.org/10.1201/9780429459016>.

Perpiñán, Oscar, and Robert Hijmans. 2023. *rasterVis*.
<https://oscarperpinan.github.io/rastervis/>.

Quoss, Luise, Nestor Fernandez, Christian Langer, Jose Valdez, and
Henrique Miguel Pereira. 2021. *ebvcube: Working with netCDF for
Essential Biodiversity Variables*. Germany: German Centre for
Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig.
<https://github.com/EBVcube/ebvcube>.

R Core Team. 2023. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

Thuiller, Wilfried, Damien Georges, Maya Gueguen, Robin Engler, Frank
Breiner, Bruno Lafourcade, and Remi Patin. 2023. *biomod2: Ensemble
Platform for Species Distribution Modeling*.

Wickham, Hadley. 2016. *ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

———. 2019. “S3.” In *Advanced R*, 2nd ed. Chapman and Hall/CRC.
<https://doi.org/10.1201/9781351201315>.

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the tidyverse.” *Journal of Open Source Software* 4 (43):
1686. <https://doi.org/10.21105/joss.01686>.

Wickham, Hadley, Romain François, Lionel Henry, Kirill Müller, and Davis
Vaughan. 2023. “dplyr: A Grammar of Data Manipulation.”
<https://dplyr.tidyverse.org>.

Wickham, Hadley, Davis Vaughan, and Maximilian Girlich. 2023. “tidyr:
Tidy Messy Data.” <https://tidyr.tidyverse.org>.

Wickham, Hadley, Claus O. Wilke, and Thomas Lin Pedersen. 2022.
*isoband: Generate Isolines and Isobands from Regularly Spaced Elevation
Grids*. <https://CRAN.R-project.org/package=isoband>.

------------------------------------------------------------------------

1.  The term `geoms` refers to geometric objects, and `stats` refers to
    statistical transformations, following the naming conventions of
    **ggplot2**

2.  CRS; Coordinate reference system
