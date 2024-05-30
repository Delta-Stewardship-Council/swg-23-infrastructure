## ------------------------------------------ ##
#   Infrastructure -- GHG Accounting
## ------------------------------------------ ##

# Life stage: Early Analysis

# Script author(s): Karrin Alstad
# Date: 2024/05/23


# Purpose:
# Using data from Vaughn et al and from ARS to try to represent current GHG fluxes for study area
# Then divide by leveed area

# load packages
library(terra)
library(sf)
library(ggplot2)
library(dplyr)


# set to my directory for now
# flux_2018_2027_tCO2e_ac_M_OM_M <- rast("data-raw/Vaughn_etal_and_ARS/Vaughn_2024_2018-2027/flux_2018_2027_tCO2e_ac_M_OM_M.tif")
# flux_2018_2027_tCO2e <- project(flux_2018_2027_tCO2e_ac_M_OM_M, "EPSG:3310")
# plot(flux_2018_2027_tCO2e)

flux_1yr_M_OM_M <- rast("data-raw/Vaughn_etal_and_ARS/flux_1yr_M_OM_M_extent_wBacon_RF.tif")
sub_1yr_OM_M <- rast("data-raw/Vaughn_etal_and_ARS/sub_1yr_OM_M_extent_wBacon.tif")

tidal <- read_sf("data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp")%>%
  st_transform(crs = 3310)
crs(tidal)

tidal %>%
  ggplot() +
  geom_sf()

tidal_tv <- terra::vect(tidal)
plot(tidal_tv)

tidal_boundary_extent <- ext(tidal_tv) |>
  vect()

plot(flux_1yr_M_OM_M)
lines(tidal_boundary_extent)
lines(tidal_boundary_extent, col = "blue")

#mask
cropped_masked <- mask(flux_1yr_M_OM_M , tidal_tv)
cropped_masked

# cropped_masked <- mask(flux_1yr_M_OM_M , tidal_tv)
# cropped_masked


#plot
plot(cropped_masked)
lines(tidal_tv)

#plot
# question: what are the units? 
plot(flux_1yr_M_OM_M)
lines(tidal)

#plot
# question: what are the units? 
plot(sub_1yr_OM_M)
lines(tidal)

# Next: slice up by leveed area and average raster values within ...

leveeAreas <- read_sf("data-clean/shapefiles/fixedLevees/leveedAreas.shp")
crs(leveeAreas)
plot(leveeAreas$LMA)
str(leveeAreas)
dim(leveeAreas)
width(leveeAreas)
nrow(leveeAreas)

crop(sub_1yr_OM_M, leveeAreas, mask = TRUE)



# example

library(sf)

# Read raster dataset
raster_data <- flux_1yr_M_OM_M

# Read vector shape data
vector_data <- leveeAreas

# this plot demonstrates teh three datasets
#plot(leveeAreas[100, ])


test <- crop(raster_data, leveeAreas[100, ], mask = TRUE)
plot(test)



# Loop through each polygon in the vector data and clip the raster
clipped_rasters <- list()
for (i in 1:nrow(vector_data)) {
  polygon <- vector_data[i, ]
  clipped_raster <- crop(raster_data, polygon, na.rm = TRUE)
  clipped_rasters[[i]] <- clipped_raster
  print(paste("Clipped raster", i, "completed."))
}

# Process or save the clipped raster data as needed
# For example, you can save each clipped raster to separate files
for (i in 1:length(clipped_rasters)) {
  # Preprocess to handle missing values
  clipped_raster <- clipped_rasters[[i]]
  clipped_raster[is.na(clipped_raster)] <- 0  # Replace missing values with 0
  # Save the raster
  writeRaster(clipped_raster, filename = paste0("clipped_raster_", i, ".tif"), overwrite = TRUE)
}













