# Purpose
# This script will merge the Natural Habitat Managed Wetlands shape file to
# the leveed area polygons. The end goal will be to find overlaping 
# habitat types within each leveed area and quantifying this overlapping area.

# SUMMARY
# Shape file of interest: intersectionDF
# Column metadata:
# LMA =  levee area from the leveedAreas shape file
# PM_LandTyp = habitat type of interest
# Acres = area of the land type, already calculated
# overlappedArea = our calculation of the overlapping area between the habitat of
# interest and the leveed area (LMA)
# lmaArea = area of the leveed area
# geometry = geometry layer of **OVERLAPPING** habitat type and our leveed polygons
# lmaGeometry = geometry layer of the leveed area

# You'll want to focus on PM_LandTyp, overlappedArea, and geometry
# These are the land types, overlapping acreages, and geometry of the overlapped areas

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# Workflow ----------------------------------------------------------------

# 0. Download managed wetland shapefile from KNB
# 1. Read in the levee polygons
# 2. Read in the natural habitat managed wetland polygons
# 3. Per leveed area, find overlap to the wetland polygon
# 4. Calculate the overlapped area

## Downloads Modern_Habitat_LandExtend_2016.shp from KNB
knb_url <- "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A95d26b1e-0e44-46f0-8580-cabbc942fcb2"

temp_file <- tempfile()

download.file(url = knb_url, destfile = temp_file)

unzip(temp_file, exdir = 'data-raw/shapefiles/managedWetlands')

unlink(temp_file)

# Reading the polygons, 1, 2 ----------------------------------------------
managedWetlands <- st_read(file.path("data-raw", "shapefiles", "managedWetlands", 
                                     "Modern_Habitat_LandExtend_2016.shp")) %>% 
  st_transform(crs = 3310) %>% 
  group_by(PM_LandTyp) %>% 
  summarise(geometry = st_union(geometry),
            habitatArea = st_area(geometry) %>% 
              units::set_units(acre) %>% 
              as.numeric())

leveedAreas <- st_read(file.path("data-clean", "shapefiles", "fixedLevees", "leveedAreas.shp")) %>% 
  group_by(LMA) %>%
  summarise(geometry = st_union(geometry),
            lmaArea = st_area(geometry) %>% 
              units::set_units(acre) %>% 
              as.numeric()) %>%
  ungroup()

# Calculating overlap and the area 3, 4 -----------------------------------

intersectionDF <- st_intersection(leveedAreas, managedWetlands) %>% 
  mutate(overlappedArea = units::set_units(st_area(geometry), acre))

# Checking the calculations -----------------------------------------------
# Each calculated overlap should be:
# 1. Less than the full extent of the habitat type
# 2. When summed across an LMA, be equal to or less than the total area of LMA

combinedCheck <- intersectionDF %>% 
  mutate(lessThanLMA = as.numeric(overlappedArea) <= lmaArea,
         lessThanHabitatAcres = as.numeric(overlappedArea) <= habitatArea) %>% 
  group_by(LMA) %>% 
  mutate(allLessThanLMA = ifelse((sum(as.numeric(overlappedArea)) <= (lmaArea + 0.00001)), T, F)) %>% 
  ungroup()

apply(data.frame(combinedCheck)[, c("lessThanLMA", "lessThanHabitatAcres", "allLessThanLMA")], 2, 
      function(x) sum(!x, na.rm = T))
# All checks out.
# Should be all 0s.

# Visualizing -------------------------------------------------------------
leveedAreaPlot <- ggplot(leveedAreas) +
  geom_sf(fill = NA, color = "grey70") +
  # guides(fill = "none") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(linewidth = 0.25))

landTypes <- na.omit(unique(combinedCheck$PM_LandTyp))

# Use any color palette you'd like
set.seed(135)
colorsDF <- data.frame(
  colors = hues::iwanthue(11, 0, 360, 40, 70, 15, 85, random = T),
  landType = landTypes
)

landTypePlots <- lapply(landTypes,
               function(x) {
                 dat <- filter(combinedCheck, PM_LandTyp == x)
                 
                 totalAcreage <- as.numeric(sum(dat$overlappedArea))
                 
                 leveedAreaPlot +
                   geom_sf(data = dat,
                           aes(fill = PM_LandTyp, color = PM_LandTyp), size = 1) +
                   scale_fill_manual(values = filter(colorsDF, landType == x)$colors) +
                   scale_color_manual(values = filter(colorsDF, landType == x)$colors) +
                   labs(title = paste0("Habitat type: ", x),
                        subtitle = paste0("Total acreage: ", scales::label_comma(accuracy = 0.01)(totalAcreage)))
               }) %>% 
  setNames(landTypes)

library(Cairo)

# Loop through the list and save each plot

for (i in seq_along(landTypePlots)) {
  CairoPNG(file = file.path("figures", "managedWetlands", paste0(names(landTypePlots)[i], ".png")), 
           width = 15, height = 10, units = "in", res = 300)
  print(landTypePlots[[i]])
  dev.off()
}

# Saving the shapefile ----------------------------------------------------

# NOT doing this because the file is excessively large, ~279 mb. Will need to simply
# run this code to get the shape file of interest
dir.create(file.path("data-clean", "shapefiles", "managedWetlands"), showWarnings = FALSE)

st_write(intersectionDF,
         file.path("data-clean", "shapefiles", "managedWetlands", "managedWetlands.shp"),
         append = F)

