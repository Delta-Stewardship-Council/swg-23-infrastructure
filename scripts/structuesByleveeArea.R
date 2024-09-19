## Creating data layer with type and value of structures within each levee area

## Set up ----
library(sf)
library(dyplr)

## read data ---

# structure data
structue_data <- st_read(file.path("data-clean", "shapefiles", "nationalStructureInventory", "nsi_2022_delta_area.shp"))

# levee areas
levee_areas <- st_read(file.path("data-clean", "shapefiles", "fixedLevees", "leveedAreas.shp")) %>% 
  group_by(LMA) %>%
  summarise(geometry = st_union(geometry))

plot(levee_areas["geometry"])

## select the variables we need from structure data ----
structure_value <- structue_data %>% 
  select(cbfips, occtype, sqft, val_cont, val_vehic, val_struct) %>% 
  mutate(total_value = sum(val_cont, val_struct, val_vehic))

## Check for crs
st_crs(levee_areas)
st_crs(structure_value)

## transforming structur data to California alberts
structure_nad83 <- st_transform(structue_value,
                                crs = 3310)
st_crs(structure_nad83)

## Calculate structures per leveed polygon, calculate overlap ----
structure_per_levee <- st_intersection(structure_nad83, levee_areas) 
  
total_value_area <- structur_per_levee %>% 
  group_by(LMA) %>% 
  summarise(geometry = st_union(geometry),
            value_area = sum(total_value))
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

## The value plots buy point and not bue polygon. 


