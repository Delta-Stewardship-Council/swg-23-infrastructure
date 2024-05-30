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
plot(leveeAreas, labels="LMA")

raster_proj <- st_crs(flux_1yr_M_OM_M)$epsg
vector_proj <- st_crs(leveeAreas)$epsg


plot(flux_1yr_M_OM_M)
lines(leveeAreas)


# rename raster dataset
raster_data <- flux_1yr_M_OM_M

# rename vector shape data
vector_data <- leveeAreas



library(sf)
library(terra)


# Ensure vector data is valid and not empty
vector_data <- vector_data[!st_is_empty(vector_data), ]

# # Check and align CRS
# if (st_crs(vector_data) != crs(raster_data)) {
#   vector_data <- st_transform(vector_data, crs(raster_data))
# }

# Convert sf object to SpatVector
vector_data_vect <- vect(vector_data)

# Use extract function to clip the raster with the polygons
clipped_rasters <- extract(raster_data, vector_data_vect, fun = mean, na.rm=TRUE)


# Use extract function to clip the raster with the polygons (does not need to be a SpatVector)
clipped_rasters <- extract(raster_data, vector_data, fun = mean, na.rm=TRUE)






















