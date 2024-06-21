# Overlapping polygons for probability of levee failure ----


# Purpose
# Following the steps in managedWetlands.R, this script will merge the probFailure.csv data (in clean-data folder) to the leveed area polygons. The end goal will be to find overlapping areas within each leveed area and quantifying the prob of failure this overlapping area.

# SUMMARY
# ADD SUMMARY STEPS

# Workflow ----
# 1. Read in the probability of failure data (clean-data/randProbFailure/propFailure.csv)
# 2. Read RAND polygons data (data-raw/shapefiles/deltaLeveeAreas/i17_Delta_Levees_Centerlines_2017.shp)
# 3. Read in the levee polygons (clean-data/shapefiles/fixedLevees/leveedAreas.shp)
# 4. Decide under which scenario we will calculate the prob-of failure and filter probFail data
# 5. Add spatial component to probFail! (NOT SURE HOW TO GET HERE)
# 6. Per leveed area, find overlap to the pro-fail polygon
# 7. Calculate the overlapped area


# Load libraries ----
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidycensus)
library(janitor)

# Read data (1,2,3) ----

probFail <- read_csv(file.path("data-clean", "randProbFailure", "probFailure.csv"))

randLeveeAreas <- st_read(file.path("data-raw", "shapefiles", "deltaLeveeAreas", "i17_Delta_Levees_Centerlines_2017.shp"))

leveedAreas <- st_read(file.path("data-clean", "shapefiles", "fixedLevees", "leveedAreas.shp"))


# Filter for probFailure Scenario we want (4) ----

# Here we are choosing the simple and most basic scenario for now! We can later decide if we want to show different scenarios in the final results.

probFail_baseline <- probFail %>% 
  filter(SLR_Condition == "baseline",
         LF_Curve == 4,
         Discount_Rate == 0.00) %>% 
  select(islandID, Island.or.Tract, Year, hydrologicFailure, seismicFailure, leveeFailure) %>% 
  clean_names() %>% 
  mutate(island_or_tract = str_to_lower(island_or_tract))


# Adding spatial component to probFail data by joining to randLeveeArea
# NOTE: I'm not sure if this is the right thing to do here. But for now I'll go with it and make sure to get feedback form group.

# There are 142 islands in the probFail data and 135 island in the spatial randLevees Ares. 

# Checking for matches

randLeveeAreas_clean <- randLeveeAreas %>% 
  clean_names() %>% 
  select(objectid, lma, lma_number, levee_type, levee_cond) %>% 
  mutate(lma = str_to_lower(lma))


# Test to see which island have a direct match and which ones don't
probFail_islands <- probFail_baseline %>% 
  select(island_id, island_or_tract) %>% 
  mutate(probFail_id =  1:n())

randLevee_islands <- randLeveeAreas_clean %>% 
  select(objectid, lma, lma_number) %>% 
  mutate(randLevee_id = 1:n())








