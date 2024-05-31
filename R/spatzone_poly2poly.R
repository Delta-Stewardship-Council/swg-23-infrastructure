# To perform spatial zonal statistics on vector polygons using sf and terra packages in R. 
# If you have two vector polygon layers and you want to derive summaries based on their spatial relationships, 
# you can use spatial joins and the aggregate function to achieve this.



# SF Package
# Load the necessary libraries and data:

library(sf)
library(dplyr)

# Read your vector polygon data
vector_data_1 <- st_read("data-clean/shapefiles/fixedLevees/leveedAreas.shp")
vector_data_2 <- st_read("data-raw/shapefiles/habitatDelta1977/i07_Habitat_Delta_1977.shp")

plot(vector_data_2)


# Ensure CRS alignment:
 
if (st_crs(vector_data_1) != st_crs(vector_data_2)) {
  vector_data_2 <- st_transform(vector_data_2, st_crs(vector_data_1))
}

# Perform a spatial join:
# Use st_join to perform a spatial join, where each polygon in vector_data_1 
# will be associated with overlapping polygons in vector_data_2.

joined_data <- st_join(vector_data_1, vector_data_2, join = st_intersects)


# Calculate zonal statistics:
# Use the dplyr package to summarize the joined data. For example, if you want to calculate the area of intersections
# or any other attribute-based summaries:

# Example: Summarize the area of intersections
joined_data$intersection_area <- st_area(joined_data)

# Summarize the response variable
summary_stats <- joined_data %>%
  group_by(ID_1) %>%  # Replace with leveed area name variable
  summarize(total_response = sum(response, na.rm = TRUE))

# Print the summary statistics
print(summary_stats)











# TERRA package
library(terra)

# Read your vector polygon data
vector_data_1 <- vect("path/to/your/first_vector_file.shp")
vector_data_2 <- vect("path/to/your/second_vector_file.shp")

# Ensure CRS alignment:

if (crs(vector_data_1) != crs(vector_data_2)) {
  vector_data_2 <- project(vector_data_2, crs(vector_data_1))
}

# Perform a spatial intersection:
# Use the intersect function to get the intersections of the polygons:
 
intersections <- intersect(vector_data_1, vector_data_2)


# Calculate zonal statistics:
# Use the terra package to calculate summary statistics:

# Example: Summarize the area of intersections
intersections$area <- expanse(intersections)

# Aggregate summaries
summary_stats <- aggregate(intersections, by = "ID_1", fun = sum, na.rm = TRUE)

