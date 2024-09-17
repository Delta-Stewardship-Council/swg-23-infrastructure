# Install and load required packages
library(dplyr)
library(rvest)
library(httr)
library(terra)
library(sf)

# Step 1: Scrape the download site for the newest dataset
downloadUrl <- "https://www.nass.usda.gov/Research_and_Science/Cropland/Release/"
webpage <- read_html(downloadUrl)
links <- webpage %>% 
  html_nodes("a") %>% 
  html_attr("href")
zipLinks <- links[grep("\\.zip$", links)]
# Assuming the first link is the newest
newestZip <- zipLinks[1]  
cat("The newest file appears to be", shQuote(newestZip))
# Check this assumption

# Step 2: Download the file
tempFile <- tempfile(fileext = ".zip")
download.file(paste0(downloadUrl, newestZip), tempFile, mode = "wb")

# Step 3: Unzip the file
unzipDir <- "data-raw/shapefiles/croplandDataLayer"
dir.create(unzipDir, showWarnings = FALSE)
unzip(tempFile, exdir = unzipDir)

# Step 4: Identify the .tif file
tifFiles <- list.files(unzipDir, pattern = "\\.tif$", full.names = TRUE)

# Step 5: Read in the .tif file
raster <- rast(tifFiles)
# Plotting it
plot(raster)
# Identify the grid size of each cell
cellSize <- res(raster)
print(paste("Cell size:", cellSize[1], "x", cellSize[2], "units"))

# Step 6: Filter for only the tidal Delta
# First, we need to get California's boundary
deltaPath <- "data-raw/shapefiles/deltaBoundary/SacSJ_TidallyInfluencedBoundary/Tidally_Influenced_Delta_SacSJ.shp"
deltaBoundary <- read_sf(deltaPath) %>% 
  st_transform(crs(raster))

# Crop the raster to California's extent
deltaRaster <- crop(raster, deltaBoundary) %>% 
  # mask(deltaBoundary) %>% # Can probably mask to the area to make operations faster
  # # But have to be careful about the masking process including or not including shared grids on the perimeter
  # # Probably not an issue since it WILL include any grid that is within the area, to which intersect
  # # will just find the overlap
  # Convert to EPSG3310 for spatial calculations
  project("EPSG:3310")

# Step 7: Convert raster to vector format
deltaCropPolygons <- as.polygons(deltaRaster, values = T)
# deltaCropPolygons <- as.polygons(deltaRaster, dissolve = FALSE, values = TRUE)

# Convert to sf object
deltaCropSF <- st_as_sf(deltaCropPolygons)

# Step 8: Read in the levee polygons
leveedAreas <- st_read("data-clean/shapefiles/fixedLevees/leveedAreas.shp") %>% 
  group_by(LMA) %>%
  summarise(geometry = st_union(geometry),
            area = st_area(geometry) %>% 
              units::set_units(acre) %>% 
              as.numeric()) %>%
  ungroup()

# Step 9: Calculate cropland area per leveed polygon, calculate overlap
cropPerLevee <- st_intersection(deltaCropSF, leveedAreas) %>% 
  mutate(cropArea = as.numeric(st_area(geometry) %>% 
                                 units::set_units(acre)))

# Step 10: Quickly check area of cropland before and after conversion
options(scipen = 99999)
sumAreaVector <- st_area(deltaCropSF)
sumCompare <- expanse(deltaRaster, byValue = T) %>% 
  mutate(vectorArea = as.numeric(sumAreaVector),
         same = all.equal(area, vectorArea))
# All T, conversion appears to be working

# Save as shapefile
cleanedDataDirectory <- "data-clean/shapefiles/deltaCropland"
dir.create(cleanedDataDirectory, showWarnings = FALSE)
st_write(cropPerLevee, file.path(cleanedDataDirectory, "deltaCropland.shp"), delete_layer = TRUE)
