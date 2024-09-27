## Creating data layer with type and value of structures within each levee area

## Set up ----
library(sf)
library(dplyr)

## read data ---

# structure data
structure_data <- st_read(file.path("data-clean", "shapefiles", "nationalStructureInventory", "nsi_2022_delta_area.shp"))

# levee areas
levee_areas <- st_read(file.path("data-clean", "shapefiles", "fixedLevees", "leveedAreas.shp")) %>% 
  group_by(LMA) %>%
  summarise(geometry = st_union(geometry))

plot(levee_areas["geometry"])

## select the variables we need from structure data ----
structure_value <- structure_data %>% 
  select(fd_id, cbfips, occtype, sqft, val_cont, val_vehic, val_struct) %>% 
  mutate(total_value = sum(val_cont, val_struct, val_vehic))

## Check for crs
st_crs(levee_areas)
st_crs(structure_value)

## transforming structur data to California alberts
structure_nad83 <- st_transform(structure_value,
                                crs = 3310)
st_crs(structure_nad83)

## Calculate structures per leveed polygon, calculate overlap ----
structure_per_levee <- st_intersection(structure_nad83, levee_areas) 
  
total_value_area <- structure_per_levee %>% 
  st_drop_geometry() %>% 
  group_by(LMA) %>% 
  summarise(structure_value_area = sum(total_value))
## Note that not all LM have a structure value. There are only 93 obs in total_value_area df.

## What are the missing LMA ares adn why?
value_lma <- total_value_area %>% 
  select(LMA) %>% 
  st_drop_geometry() %>% 
  pull()

missing_lma <- levee_areas %>% 
  filter(!LMA %in% value_lma)
plot(total_value_area["value_area"])
## Most of them are island that maybe do not have structure data?

## matching each total value to a polygon instead of a point

total_value_polygon <- levee_areas %>% 
  left_join(total_value_area, by = "LMA") %>% 
  # Multiplying value to bring it account for inflation and bring it to 2024 dollar value
  mutate(total_value_trill = (structure_value_area*1.14)/10^12)


plot(total_value_polygon["structure_value_area"])

## Save shapefile with total 2024 value per area
write_sf(total_value_polygon, "data-clean/shapefiles/nationalStructureInventory/nsi_2024_total_value_levee_area.shp", overwrite = TRUE)


