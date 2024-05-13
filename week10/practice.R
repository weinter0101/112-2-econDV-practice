library(tidyverse)
library(sf)
# Reading a shapefile -----
shapeData <- st_read("C:/Users/chewei/Documents/GitHub/112-2-econDV-practice/data/mapdata202301070205/COUNTY_MOI_1090820.shp")
glimpse(shapeData)

class(shapeData)

# Simplifying shapeData simple feature -----
simplified_shape <- st_simplify(shapeData,
                                preserveTopology = TRUE, 
                                dTolerance = 2)
glimpse(simplified_shape)

ggplot()+
  geom_sf(
    data=simplified_shape
  )

# Obtaining bounding box -----
bbox <- st_bbox(simplified_shape)
bbox

bbox["ymin"] <- 21

simplified_shape <- 
  st_crop(simplified_shape,
          bbox)

ggplot()+
  geom_sf(
    data=simplified_shape
  )

object.size(simplified_shape)

# Filtering out COUNTYNAME = "新北市" -----
simplified_shape2 <- simplified_shape %>% 
  filter(COUNTYNAME == "新北市")
class(simplified_shape2)

ggplot()+
  geom_sf(
    data=simplified_shape2,
    fill="red",color="blue",
    linewidth=2
  )

# Creating a column "zone" with random assignment -----
set.seed(123)  # Setting seed for reproducibility
simplified_shape <- simplified_shape %>%
  mutate(zone = sample(c("north", "south", "east", "west"), size = n(), replace = TRUE))
glimpse(simplified_shape)

ggplot()+
  geom_sf(
    data=simplified_shape,
    mapping=aes(
      fill=zone
    )
  )

# Plotting simplified_shape with filled color determined by zone column -----
plot <- ggplot(simplified_shape) +
  geom_sf(aes(fill = zone)) +
  theme_minimal() +
  scale_fill_manual(values = c("north" = "blue", "south" = "green", "east" = "red", "west" = "yellow")) +
  labs(title = "Simplified Shape with Zone Coloring", subtitle = "Zone") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 8)) +
  theme(axis.line.x = element_line(color = "black", size = 0.5)) +
  labs(caption = "Data Source: your_source")

print(plot)