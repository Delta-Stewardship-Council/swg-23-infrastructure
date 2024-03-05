# This explores the levee centerlines
source(file.path("R", "createLeveePolygons.R"))

library(mapview)
library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)

# Ruleset:

# 1. 2017 levee centerline shapefile, polygonize these leveed areas
# 1b. shared levee lines
# 2. Integrate the suisun marsh dataset
# 3. NLD centerline shapefile, use these additional centerlines to complete uncompleted areas
# 4. Integrate the Delta border, tidally influenced version for now, and find
# shortest distance of bordering incomplete polygons to the border

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

# areas$RANDModel <- lapply(polygons$levees, "[[", 1) %>% 
#   bind_rows() %>% 
#   arrange(-acres) %>% 
#   mutate(LMA = tolower(LMA)) %>% 
#   full_join(read.csv(file.path("data-raw", "csvFiles", "DLIS.IslandBasics.csv"), check.names = F) %>% 
#               transmute(LMA = tolower(`Field Name`),
#                         acresDLIS = `Area (acres)`,
#                         priorityIsland = `Priority Island`,
#                         leveed = Leveed,
#                         active = `Active for DLIS`),
#             by = "LMA") %>% 
#   mutate(difference = acres - acresDLIS)

# Loading in the DLIS islands shape file to compare areas highlighted
shapefiles$DLISLevees <- st_read(file.path("data-raw", "shapefiles", "RevisedIslands_160912_AllIslands", 
                                           "RevisedIslands_160912_AllIslands.shp"))

# Rule 1b: shared levee lines ---------------------------------------------
sharedLevee <- list()

# Tyler Island
sharedLevee$`Tyler Island` <- data.frame(LMA = "Tyler Island",
                                         sharedLevees(
  "Tyler Island", c(414, 415, 417)
))

# Jones Tract
sharedLevee$`Jones Tract` <- data.frame(LMA = "Jones Tract",
                                        st_intersection(
  sharedLevees("lower jones tract",
               c(filter(shapefiles$levees, LMA == "Upper Jones Tract") %>%
                   pull(OBJECTID),
                 255)),
  st_as_sfc(st_bbox(snapCenterlines(filter(shapefiles$levees,
                                           LMA %in% c("Lower Jones Tract", "Upper Jones Tract")),
                                    10), crs = 3310))
))

# Roberts Island
sharedLevee$`Roberts Island` <- data.frame(LMA = "Roberts Island",
                                           shapefiles$levees %>% 
  filter(LMA %in% c("Middle Roberts Island", "Upper Roberts Island") |
           OBJECTID %in% c(256)) %>% 
  bind_rows(shapefiles$levees %>% 
              filter(OBJECTID == 255) %>% 
              st_cast("LINESTRING") %>% 
              # Plot this if you're curious if this is the right line or not
              .[10, ] %>% 
              lwgeom::st_linesubstring(0, 0.1)) %>% 
  st_union() %>% 
  st_polygonize())

# Wright-Elmwood Tract
sharedLevee$`Wright-Elmwood Tract` <- data.frame(LMA = "Wright-Elmwood Tract",
                                                 sharedLevees("wright-elmwood tract", c(341, 342)))

# Little Egbert Island
sharedLevee$`Little Egbert Island` <- data.frame(LMA = "Little Egbert Island",
                                                 sharedLevees("little egbert island", 193))

# Prospect Island
sharedLevee$`Prospect Island` <- data.frame(LMA = "Prospect Island",
                                            sharedLevees("prospect island",
                                              c(473, 474, 283, 58)))

polygons$rule1b <- bind_rows(
  lapply(polygons$leveesAll, function(x) {
    if (is.null(x$p)) {
      data.frame(LMA = x$df$LMA)
    } else {
      data.frame(LMA = x$df$LMA,
                 x$polygon)
    }
  }) %>% 
    bind_rows() %>% 
    mutate(dataset = "dwrCenterlines") %>% 
    st_sf(),
  bind_rows(sharedLevee) %>% 
    mutate(dataset = "sharedLevee"))

ggplot(polygons$rule1b, aes(fill = dataset)) +
  geom_sf()

# Rule 2 Suisun Marsh integration -----------------------------------------

shapefiles$`Suisun Marsh` <- st_read(file.path("data-raw", "shapefiles", "suisunMarsh", "i03_SuisunMarshBoundary.shp")) %>% 
  st_transform(crs = 3310) %>% 
  filter(LOCATION != "Primary (Water)") %>% 
  mutate(LMA = "Suisun Marsh") %>% 
  st_union() %>% 
  data.frame(LMA = "Suisun Marsh", .)

polygons$ruleTwo <- bind_rows(
  polygons$rule1b,
  shapefiles$`Suisun Marsh` %>% 
    mutate(dataset = "suisunMarsh")
)

ggplot(polygons$ruleTwo, aes(fill = dataset)) +
  geom_sf()

# Rule 3: NLD centerline --------------------------------------------------

shapefiles$nld <- st_read(file.path("data-raw", "shapefiles", "nationalLeveeDatabase", "systemLines", "POLYLINE.shp")) %>% 
  st_transform(crs = 3310) %>% 
  mutate(dataset = "nld")

# polygons$ruleTwo %>% 
#   st_cast("MULTILINESTRING") %>% 
#   bind_rows(shapefiles$levees %>% 
#               mutate(lines = "centerLine"),
#             st_intersection(st_as_sfc(st_bbox(shapefiles$levees)), 
#                             shapefiles$nld) %>% 
#               st_sf() %>% 
#               mutate(lines = "nld")) %>% 
#   mapview(zcol = "lines")

# Rule 4: Shortest distance to the functional Delta -----------------------

shapefiles$functionalDelta <- st_read(file.path("data-raw", "shapefiles", "deltaBoundary", 
                                              "SacSJ_TidallyInfluencedBoundary", "Tidally_Influenced_Delta_SacSJ.shp")) %>% 
  st_transform(crs = 3310)

# Steps: 
# 1: join the boundary file
# 2: identify the applicable lines
# 3: st_boundary to find the end points
# 4: find shortest distance to the boundary

# Joining the boundary file
pal <- colorFactor("inferno", domain = polygons$ruleTwo$LMA)

leaflet() %>% 
  addPolygons(
    data = polygons$ruleTwo %>% 
      st_cast("POLYGON") %>% 
      st_transform(4326),
    stroke = F,
    fillColor = ~pal(LMA),
    fillOpacity = 0.55, smoothFactor = 0.5,
    popup = paste0(polygons$ruleTwo$LMA)
  ) %>% 
  # addPolylines(
  #   data = shapefiles$DLISLevees %>% 
  #     st_cast("MULTILINESTRING") %>% 
  #     st_transform(4326),
  #   weight = 2, color = "black"
  # ) %>% 
  addPolylines(
    data = shapefiles$nld %>% 
      st_transform(4326),
    weight = 6, color = "black",
    popup = paste0(shapefiles$nld$name),
    highlightOptions = highlightOptions(color = "white", bringToFront = TRUE)
  ) %>% 
  addPolylines(
    data = shapefiles$levees %>% 
      st_transform(4326),
    weight = 6, color = "blue"
  ) %>% 
  addPolylines(
    data = shapefiles$functionalDelta %>% 
                       mutate(dataset = "functionalDelta") %>% 
      st_transform(4326),
    weight = 6, color = "firebrick"
  ) %>% 
  addProviderTiles("Esri.WorldImagery")

# Starting from the top, immediately east of west sacramento and creating the border
# nearest to the functional border

source(file.path("scripts", "leveeCorrectionScripts", "boundaryPolygons.R"))

polygons$rule4 <- bind_rows(
  polygons$ruleTwo,
  nceas
)

# Saving the shapefile ----------------------------------------------------

bind_rows(
  polygons$rule1b %>% 
    mutate(ruleset = "one"),
  shapefiles$`Suisun Marsh` %>% 
    mutate(dataset = "dwrSuisunMarsh",
           ruleset = "two"),
  bind_rows(nceas) %>% 
    mutate(ruleset = "four")
) %>% {
  # Some duplicated LMAs due to failed polygonize on original centerline dataset
  # here, fixed in rule 1b
  duplicatedLMAs <- janitor::get_dupes(., "LMA") %>% 
    filter(dataset == "sharedLevee")
  
  filter(., !LMA %in% unique(duplicatedLMAs$LMA)) %>% 
    bind_rows(duplicatedLMAs)
} %>% 
  split(.$LMA) %>% 
  {
  st_write(
    lapply(., function(x) {
      if (!is.null(x$geometry)) {
        if (any(st_geometry_type(x$geometry) == "GEOMETRYCOLLECTION")) {
          data.frame(LMA = x$LMA,
                     dataset = x$dataset,
                     ruleset = x$ruleset,
                     x$geometry %>% 
                       .[[1]] %>% 
                       st_multipolygon() %>% 
                       st_sfc(crs = 3310))
        } else {
          data.frame(LMA = x$LMA,
                     dataset = x$dataset,
                     ruleset = x$ruleset,
                     x$geometry %>% 
                       st_cast("MULTIPOLYGON") %>% 
                       .[[1]] %>% 
                       st_multipolygon() %>% 
                       st_sfc(crs = 3310))
        }
      } else {
        data.frame(LMA = x$LMA,
                   dataset = x$dataset,
                   ruleset = x$ruleset,
                   st_sfc(st_multipolygon(list()), crs = 3310))
      }
    }) %>% 
      bind_rows() %>% 
      mutate(geometry = st_make_valid(geometry)), 
    file.path("data-clean", "shapefiles", "fixedLevees", "leveedAreas.shp"), 
    append = F
  )
}
