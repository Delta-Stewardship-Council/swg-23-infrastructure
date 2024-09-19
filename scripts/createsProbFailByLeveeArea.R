# This scripts creates the format for probability levee failure data withing each levee areas based on the RAND prob of Failure data and the fixed levee island


# Load Packages ----
library(tidyverse)
library(janitor)
library(sf)


# Read data ----
prob_fail <- read_csv(file.path("data-clean", "randProbFailure", "probFailure.csv")) %>% 
  clean_names()

leveed_areas <- st_read(file.path("data-clean", "shapefiles", "fixedLevees", "leveedAreas.shp")) %>% 
  clean_names() %>% 
  mutate(lma = str_to_lower(lma))


# Filter prob_fail to baseline scenario ----
prob_fail_baseline <- prob_fail %>% 
  filter(slr_condition == "baseline",
         lf_curve == 4,
         discount_rate == 0.00) %>% 
  select(island_id, island_or_tract, year, hydrologic_failure, seismic_failure, levee_failure) %>% 
  mutate(island_or_tract = str_to_lower(island_or_tract))

# Assigning a prob of failure to a each levee ares ----

# NOTE: THERE IS NOT DIRECT MATCH OF LEVEE AREA TO ISLANDS PRESERNTED IN THEN PROB OF FAILURE. 

prob_fail_area <- leveed_areas %>% 
  left_join(prob_fail_baseline, join_by(lma == island_or_tract))


plot(prob_fail_area["levee_failure"])


# save data to use in app ----
write_sf(prob_fail_area, file.path("data-clean", "shapefiles", "probFailure", "prob_fail_levee_area.shp"))





