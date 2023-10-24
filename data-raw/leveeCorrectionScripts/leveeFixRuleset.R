# This explores the levee centerlines
source(file.path("R", "createLeveePolygons.R"))

library(mapview)
library(sf)
library(ggplot2)
library(dplyr)

# Ruleset:

# 1. 2017 levee centerline shapefile, polygonize these leveed areas
# 1b. shared levee lines
# 2. Integrate the suisun marsh dataset
# 3. NLD centerline shapefile, use these additional centerlines to complete uncompleted areas
# 4. Integrate the Delta border, tidally influenced version for now
# 5. Find shortest distance of the outlying incomplete polygons to the border

#### IMPORTANT ####
# Make sure you run through at least these two sections in "ecologicalAssets.R":
# Section "Data lists"
# Section "delta levees"

shapefiles <- list()
polygons <- list()
areas <- list()

# Rule 1: 2017 levee centerline -------------------------------------------
# Use of centerline to make into polygons

shapefiles$levees <- st_read(file.path("data-raw", "shapefiles", "deltaLeveeAreas", 
                                       "i17_Delta_Levees_Centerlines_2017.shp")) %>%
  st_transform(crs = 3310)

polygons$leveesAll <- lapply(na.omit(unique(shapefiles$levees$LMA)),
                      function(x) {
                        polygon <- shapefiles$levees %>% 
                          filter(LMA == x) %>% 
                          st_union() %>% 
                          st_polygonize()
                        
                        area <- polygon %>% 
                          st_area() %>% 
                          units::set_units(acre) %>% 
                          as.numeric()
                        
                        if (area == 0) {
                          p = NULL
                        } else {
                          p <- ggplot() +
                            geom_sf(data = polygon, fill = "lightblue") +
                            labs(title = x, subtitle = paste0("Acres: ", area)) +
                            theme_minimal()
                        }
                        
                        df <- tibble(LMA = x,
                                     acres = area)
                        
                        list(df = df,
                             polygon = polygon,
                             p = p)
                      }) %>% 
  setNames(na.omit(unique(shapefiles$levees$LMA)))

areas$RANDModel <- lapply(polygons$levees, "[[", 1) %>% 
  bind_rows() %>% 
  arrange(-acres) %>% 
  mutate(LMA = tolower(LMA)) %>% 
  full_join(read.csv(file.path("data-raw", "csvFiles", "DLIS.IslandBasics.csv"), check.names = F) %>% 
              transmute(LMA = tolower(`Field Name`),
                        acresDLIS = `Area (acres)`,
                        priorityIsland = `Priority Island`,
                        leveed = Leveed,
                        active = `Active for DLIS`),
            by = "LMA") %>% 
  mutate(difference = acres - acresDLIS)

# Loading in the DLIS islands shape file to compare areas highlighted
shapefiles$DLISLevees <- st_read(file.path("data-raw", "shapefiles", "RevisedIslands_160912_AllIslands", 
                                           "RevisedIslands_160912_AllIslands.shp"))

# Rule 2 Suisun Marsh integration -----------------------------------------

polygons$`Suisun Marsh` <- st_read(file.path("data-raw", "shapefiles", "suisunMarsh", "i03_SuisunMarshBoundary.shp")) %>% 
  st_transform(crs = 3310) %>% 
  filter(LOCATION != "Primary (Water)") %>% 
  mutate(LMA = "Suisun Marsh") %>% 
  st_union() %>% 
  data.frame(LMA = "Suisun Marsh", .)

polygons$ruleTwo <- bind_rows(
  lapply(polygons$leveesAll, function(x) {
    if (is.null(x$p)) {
      data.frame(LMA = x$df$LMA)
    } else {
      data.frame(LMA = x$df$LMA,
                 x$polygon)
    }
  }) %>% 
    bind_rows() %>% 
    mutate(name = "dwrCenterlines") %>% 
    st_sf(),
  polygons$`Suisun Marsh` %>% 
    mutate(name = "suisunMarsh")
)

# Rule 3: NLD centerline --------------------------------------------------

polygons$nld <- st_read(file.path("data-raw", "shapefiles", "nationalLeveeDatabase", "systemLines", "POLYLINE.shp")) %>% 
  st_transform(crs = 3310) %>% 
  mutate(name = "nld")

polygons$ruleTwo %>% 
  ggplot(aes(fill = LMA)) +
  geom_sf(show.legend = F)

mapview(polygons$nld %>% 
          bind_rows(polygons$ruleTwo %>%
                      st_cast("MULTILINESTRING")), 
        zcol = "name", legend = F)
  

mapview(polygons$nld, zcol = "name")
