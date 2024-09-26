# Purpose: this script will attempt to place a value on the various crop types
# in the cropland dataset.

# Sources: 
# 1. the cropland dataset is from the USDA cropland dataset. See 
# 'createCroplandShapefile.R' to see how this shapefile was manipulated.
# 2. the valuation crosswalk was produced by Daya. She took the data from
# the 2022 CVFPP. Daya still searching for metadata for this dataset.

# Workflow:
# 1. Reaad in the cropland dataset and csv crosswalk
# 2. Merge the files together
# 3. Explore ways to categorize its surbey

library(sf)
library(dplyr)

# --- Read in the shapefile ---
cropland <- st_read(file.path("data-clean", "shapefiles", "deltaCropland", "deltaCropland.shp"))

# --- Read in the crosswalk ---
crosswalkCropland <- read.csv(file.path("data-raw", "csvFiles", "landUseCropValuation.csv"), strip.white = T) %>% 
  # Use long duration here; this is for floods that lasts for more than 5 days,
  # which would be our scenario in which the area is flood in perpetuity
  transmute(Clss_Nm = Crop.Name, longDuration = `Loss.acre...Long.Duration`)

# # --- Read in crop subclass from i15 ---
# cropSubclass <- read.csv(file.path("data-raw", "csvFiles", "i15cropSubclass.csv"), strip.white = T) %>% 
#   transmute(Clss_Nm = subclass,
#             class)

# --- Join the valuation to cropland ---
croplandValued <- cropland %>%
  left_join(crosswalkCropland, by = "Clss_Nm") %>% 
  # Calculate valuation by simply multiplying. Already in $ per acre
  mutate(cropValueDollars = cropAre * longDuration) %>% 
  # Adding total crop value per leveed area
  group_by(LMA) %>% 
  mutate(totalCropValue = sum(cropValueDollars, na.rm = T))

# Check to see if all crop values are used
allUsed <- cropland %>% 
  st_drop_geometry() %>%
  anti_join(crosswalkCropland, by = "Clss_Nm") %>%
  {
    if (nrow(.) == 0) T 
    else (unique(.[["Clss_Nm"]]))
  }
# Returns 0 rows; all accounted for

# # --- Join the crop subclass to cropland ---
# croplandValuedSubclass <- croplandValued %>% 
#   left_join(cropSubclass, by = "Clss_Nm")
# There are too many discrepancies. Would have to manually resolve name differences

# --- Writing the shapefile ---
if (isTRUE(allUsed)) {
  st_write(croplandValued, file.path("data-clean", "shapefiles", "deltaCropland", "deltaCroplandValued.shp"), 
           delete_layer = T)
} else {
  cat("Shapefile not written. Not all crops were used. There were:", paste(allUsed, collapse = ", "))
}
