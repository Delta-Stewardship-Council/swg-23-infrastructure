## ------------------------------------------ ##
#   Infrastructure -- GHG Accounting
## ------------------------------------------ ##

# Script author(s): Karrin Alstad
# Date: 2024/05/23

# Purpose:
# Using data from Vaughn et al to represent current GHG fluxes for study area
# Then clip by leveed area

# load raster and vector datasets

flux_1yr_M_OM_M <- rast("data-raw/Vaughn_etal_and_ARS/flux_1yr_M_OM_M_extent_wBacon_RF.tif")
plot(flux_1yr_M_OM_M)
flux_1yr_M_OM_M <- project(flux_1yr_M_OM_M, "EPSG:3310")
crs(flux_1yr_M_OM_M)

leveeAreas <- read_sf("data-clean/shapefiles/fixedLevees/leveedAreas.shp")
nrow(leveeAreas)
plot(leveeAreas)

raster_proj <- st_crs(flux_1yr_M_OM_M)$epsg
vector_proj <- st_crs(leveeAreas)$epsg

plot(flux_1yr_M_OM_M)
lines(leveeAreas)


# rename raster dataset
raster_data <- flux_1yr_M_OM_M

# rename vector shape data
vector_data <- leveeAreas


# Trying to clip the raster data to the leveed areas using a loop
# Not getting any valid overlap!

library(sf)
library(terra)

library(raster)
library(spatialEco)

# Create a raster
ras <- raster(nrows = 100, ncols = 80, xmn = 0, xmx = 1000, ymn = 0, ymx = 800)
val <- runif(ncell(ras))
values(ras) <- val

# Create a polygon within the raster extent
xym <- cbind(runif(3, 0, 1000), runif(3, 0, 800))
p <- Polygons(list(Polygon(xym)), 1)
sp <- SpatialPolygons(list(p))
spdf <- SpatialPolygonsDataFrame(sp, data = data.frame(1))

# Calculate zonal statistics using "spatialEco"
z1 <- zonal.stats(spdf, ras, stats = "mean")

# clipped_rasters now contains the results of the clipping operation

#This modification checks if there are any non-NA values in the clipped raster using the any function, which should ensure the condition is interpretable as logical.






