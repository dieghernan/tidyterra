% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_spatraster_rgb.R
\name{geom_spatraster_rgb}
\alias{geom_spatraster_rgb}
\title{Visualise \code{SpatRaster} objects as images}
\source{
Based on the \code{layer_spatial()} implementation on \CRANpkg{ggspatial} package.
Thanks to \href{https://github.com/paleolimbot}{Dewey Dunnington} and \href{https://github.com/paleolimbot/ggspatial/graphs/contributors}{ggspatial contributors}.
}
\usage{
geom_spatraster_rgb(
  mapping = aes(),
  data,
  interpolate = TRUE,
  r = 1,
  g = 2,
  b = 3,
  alpha = 1,
  maxcell = 5e+05,
  max_col_value = 255,
  ...
)
}
\arguments{
\item{mapping}{Ignored.}

\item{data}{A \code{SpatRaster} object.}

\item{interpolate}{If \code{TRUE} interpolate linearly, if \code{FALSE}
(the default) don't interpolate.}

\item{r, g, b}{Integer representing the number of layer of \code{data} to be
considered as the red (\code{r}), green (\code{g}) and blue (\code{b}) channel.}

\item{alpha}{The alpha transparency, a number in [0,1], see argument alpha in
\code{\link[grDevices]{hsv}}.}

\item{maxcell}{positive integer. Maximum number of cells to use for
the plot.}

\item{max_col_value}{Number giving the maximum of the color values range.
When this is \code{255} (the default), the result is computed most efficiently.
See \code{\link[grDevices:rgb]{grDevices::rgb()}}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}
}
\value{
A \CRANpkg{ggplot2} layer
}
\description{
This geom is used to visualise \code{SpatRaster} objects (see \code{\link[terra:rast]{terra::rast()}}) as
RGB images. The layers are combined such that they represent the red,
green and blue channel.

For plotting \code{SpatRaster} objects by layer values use \code{\link[=geom_spatraster]{geom_spatraster()}}.

The underlying implementation is based on \code{\link[ggplot2:geom_tile]{ggplot2::geom_raster()}}.
}
\section{\CRANpkg{terra} equivalent}{


\code{\link[terra:plotRGB]{terra::plotRGB()}}
}

\section{Aesthetics}{


No \code{aes()} is required. In fact, \code{aes()} will be ignored.
}

\section{Coords}{


When the \code{SpatRaster} does not present a crs (i.e.,
\code{terra::crs(rast) == ""}) the geom does not make any assumption on the
scales.

On \code{SpatRaster} that have a crs, the geom uses \code{\link[ggplot2:ggsf]{ggplot2::coord_sf()}} to
adjust the scales. That means that also the
\strong{\code{SpatRaster} may be reprojected}.
}

\examples{
\donttest{

# Tile of Castille and Leon (Spain) from OpenStreetMap
file_path <- system.file("extdata/cyl_tile.tif", package = "tidyterra")

library(terra)
tile <- rast(file_path)

library(ggplot2)


ggplot() +
  geom_spatraster_rgb(data = tile) +
  # You can use coord_sf
  coord_sf(crs = 3035)

# Combine with sf objects
vect_path <- system.file("extdata/cyl.gpkg", package = "tidyterra")

cyl_sf <- sf::st_read(vect_path)

ggplot(cyl_sf) +
  geom_spatraster_rgb(data = tile) +
  geom_sf(aes(fill = iso2)) +
  coord_sf(crs = 3857) +
  scale_fill_viridis_d(alpha = 0.7)
}
}
\seealso{
\code{\link[ggplot2:geom_tile]{ggplot2::geom_raster()}}, \code{\link[ggplot2:ggsf]{ggplot2::coord_sf()}}, \code{\link[grDevices:rgb]{grDevices::rgb()}}.

You can get also RGB tiles from the \CRANpkg{maptiles} package,
see \code{\link[maptiles:get_tiles]{maptiles::get_tiles()}}.

Other \CRANpkg{ggplot2} utils:
\code{\link{autoplot.Spat}},
\code{\link{fortify.Spat}},
\code{\link{geom_spat_contour}},
\code{\link{geom_spatraster}()},
\code{\link{ggspatvector}},
\code{\link{stat_spat_coordinates}()}
}
\concept{ggplot2.utils}
