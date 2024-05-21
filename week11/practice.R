# MRT ----
# Request subway railway data from OpenStreetMap for specified bounding box -----

library(osmdata)

# Define the bounding box: (left, bottom, right, top)
bbox <- c(121.2197, 24.8870, 122.0341, 25.2850)

# Construct the query for subway railways
query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "railway", value = "subway")

# Get the data
subway_data <- osmdata_sf(query)

#
mrt <- subway_data$osm_lines

# Glimpse the first 3 rows of the subway data
glimpse(mrt)


# NTPU -----
library(osmdata)
library(sf)

bbox <- c(121.3541, 24.9492, 121.3795, 24.9368)

query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "building", value = "university")

ntpuSocialScience <- osmdata(query)

