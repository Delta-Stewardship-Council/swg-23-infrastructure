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

tidal <- read_sf("data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp")


tidal <- sf::read_sf(file.path(infra,"data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp"))%>%
  st_transform(crs = 3310)
crs(tidal)

tidal %>%
  ggplot() +
  geom_sf()

tidal_tv <- terra::vect(tidal)
plot(tidal_tv)

#mask
sst_cropped_masked <- mask(sst_cropped, gbr_boundary)

#plot
# question: what are the units? 
plot(flux_1yr_M_OM_M)
lines(tidal)

#plot
# question: what are the units? 
plot(sub_1yr_OM_M)
lines(tidal)

# Next: slice up by leveed area and average raster values within ...








