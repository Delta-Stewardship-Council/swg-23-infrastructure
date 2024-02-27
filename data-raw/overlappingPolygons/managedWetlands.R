# Purpose
# This script will merge the Natural Habitat Managed Wetlands shape file to
# the leveed area polygons. The end goal will be to find overlaping 
# habitat types within each leveed area and quantifying this overlapping area.

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# Workflow ----------------------------------------------------------------

# 1. Read in the levee polygons
# 2. Read in the natural habitat managed wetland polygons
# 3. Per leveed area, find overlap to the wetland polygon
# 4. Calculate the overlapped area

# Reading the polygons, 1, 2 ----------------------------------------------

managedWetlands <- st_read(file.path("data-raw", "shapefiles", "managedWetlands", 
                                     "Modern_Habitat_LandExtend_2016.shp")) %>% 
  st_transform(crs = 3310)

leveedAreas <- st_read(file.path("data", "shapefiles", "fixedLevees", "leveedAreas.shp"))

# Don't know if I should keep the projection from the managed wetlands or from
# the leveed area polygon. Currently the latter as it is logistically easier
# across all datasets

# Calculating overlap and the area 3, 4 -----------------------------------

intersectionDF <- st_intersection(leveedAreasFixed, managedWetlands) %>% 
  mutate(overlappedArea = units::set_units(st_area(intersectionDF), acre)) %>% 
  full_join(data.frame(leveedAreasArea) %>% 
              select(LMA, lmaGeometry = geometry, lmaArea), by = "LMA")

# Checking the calculations -----------------------------------------------
# Each calculated overlap should be:
# 1. Less than the full extent of the habitat type
# 2. When summed across an LMA, be equal to or less than the total area of LMA

leveedAreasArea <- leveedAreasFixed %>% 
  mutate(lmaArea = as.numeric(units::set_units(st_area(geometry), acre)))

combinedCheck <- intersectionDF %>% 
  mutate(lessThanLMA = as.numeric(overlappedArea) <= lmaArea,
         lessThanHabitatAcres = as.numeric(overlappedArea) <= Acres) %>% 
  group_by(LMA) %>% 
  mutate(allLessThanLMA = ifelse((sum(as.numeric(overlappedArea)) <= (lmaArea + 0.00001)), T, F)) %>% 
  ungroup()

apply(data.frame(combinedCheck)[, c("lessThanLMA", "lessThanHabitatAcres", "allLessThanLMA")], 2, 
      function(x) sum(!x, na.rm = T))
# All checks out.

# Visualizing -------------------------------------------------------------
leveedAreaPlot <- ggplot(leveedAreasFixed) +
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
  CairoPNG(file = file.path("data", "figures", "managedWetlands", paste0(names(landTypePlots)[i], ".png")), 
           width = 15, height = 10, units = "in", res = 300)
  print(landTypePlots[[i]])
  dev.off()
}

