## Creating cropland shapefile with categories of crops
##Rationale: The list of crops has 74 types of croplands! To hard to plot them all on a map. Trinh provided a list with categories to clasify each crop type.

## Set up ----
library(sf)
library(dplyr)
library(readr)

## Read data ----
croplands <- st_read("data-clean/shapefiles/deltaCropland/deltaCropland.shp")

crop_cat <- read_csv("data-raw/csvFiles/delta_cropland_crop_type.csv") %>% 
  ## filtering our 2 types (Shrubland & Barren) that name is repeated to avoid duplicates. Choosing the nonCrop category over nlcdDerivedClasses.
  filter(!code %in% c(131, 152))

## Join DF ----
croplands_complete <- croplands %>% 
  left_join(crop_cat, by = join_by(Clss_Nm == name)) %>% 
  group_by(LMA, type) %>% 
  summarise(total_crop_area = sum(cropAre),
            geometry = st_union(geometry),
            .groups = "drop")

unique(croplands_complete$type) ## 6 different types of crops
plot(croplands_complete["type"])


## Save shapefile with total 2024 value per area
st_write(croplands_complete, "data-clean/shapefiles/deltaCropland/croplandsByType.shp")



