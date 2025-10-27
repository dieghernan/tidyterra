## code to prepare `holyroodpark` dataset goes here

# Data from https://remotesensingdata.gov.scot/
# Scotland Lidar Phase 5 DSM NT27SE
# License: https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

library(terra)
r <- rast("data-raw/dems/NT27SE_50CM_DSM_PHASE5.tif")

# Cut to fit Holyrood Park
hopark <- rast()

ext(hopark) <- c(-3.178208, -3.142817, 55.939184, 55.95418)

hopark <- project(hopark, terra::crs(r))
res(hopark) <- c(2.5, 2.5)

final_rast <- terra::project(r, hopark)
ncell(final_rast)
names(final_rast) <- "elevation"
plot(final_rast)
# Extra clip
xa <- 1000
xb <- 200
ya <- 300
yb <- 300
f2 <- as.vector(ext(final_rast))
f2_x <- f2 - c(xa, xb, ya, yb)

xtraclip <- crop(final_rast, ext(f2_x))
plot(xtraclip)
ncell(xtraclip)

unlink("data-raw/holyroodpark.tif")
unlink("vignettes/articles/holyroodpark.tif")
writeRaster(xtraclip, "data-raw/holyroodpark.tif")
writeRaster(xtraclip, "vignettes/articles/holyroodpark.tif")
