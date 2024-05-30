## ------------------------------------------ ##
#   Infrastructure -- GHG Accounting
## ------------------------------------------ ##

# Life stage: Early Analysis

# Script author(s): Karrin Alstad
# Date: 2024/05/23


# Purpose:
# Using data from Vaughn et al and from ARS to try to represent current GHG fluxes for study area
# Then divide by leveed area

# load raster and vector datasets

flux_1yr_M_OM_M <- rast("data-raw/Vaughn_etal_and_ARS/flux_1yr_M_OM_M_extent_wBacon_RF.tif")
plot(flux_1yr_M_OM_M)
flux_1yr_M_OM_M <- project(flux_1yr_M_OM_M, "EPSG:3310")
crs(flux_1yr_M_OM_M)

leveeAreas <- read_sf("data-clean/shapefiles/fixedLevees/leveedAreas.shp")
nrow(leveeAreas)
plot(leveeAreas)
crs(leveeAreas)

# rename raster dataset
raster_data <- flux_1yr_M_OM_M

# rename vector shape data
vector_data <- leveeAreas


# Trying to clip the raster data to the leveed areas using a loop
# Not getting any valid overlap!

library(sf)
library(terra)

# Define is_empty function
is_empty <- function(raster) {
  allNA(c(raster))
}


# Ensure vector data is valid and not empty
vector_data <- vector_data[!st_is_empty(vector_data), ]

# Check and align CRS
if (st_crs(vector_data) != crs(raster_data)) {
  vector_data <- st_transform(vector_data, crs(raster_data))
}

# Initialize an empty list to store clipped rasters
clipped_rasters <- list()

# Loop through each polygon in the vector data and clip the raster
for (i in 1:nrow(vector_data)) {
  polygon <- vector_data[i, ]
  
  # Ensure the polygon is valid
  if (st_is_valid(polygon)) {
    # Convert sf polygon to SpatVector
    polygon_vect <- vect(polygon)
    
    # Check if the polygon extent intersects the raster extent
    intersect_extent <- intersect(ext(polygon_vect), ext(raster_data))
    if (!is.null(intersect_extent)) {
      clipped_raster <- crop(raster_data, polygon_vect)
      # Check if clipped raster is not NULL
      if (!is.null(clipped_raster)) {
        # Check if clipped raster is not empty
        if (!any(is.na(values(clipped_raster)))) {
          clipped_rasters[[i]] <- clipped_raster
          print(paste("Clipped raster", i, "completed."))
        } else {
          print(paste("Clipped raster", i, "is empty. Skipping."))
        }
      } else {
        print(paste("Clipped raster", i, "is NULL. Skipping."))
      }
    } else {
      print(paste("Polygon", i, "does not intersect the raster extent. Skipping."))
    }
  } else {
    print(paste("Polygon", i, "is not valid. Skipping."))
  }
}

# clipped_rasters now contains the results of the clipping operation

#This modification checks if there are any non-NA values in the clipped raster using the any function, which should ensure the condition is interpretable as logical.






