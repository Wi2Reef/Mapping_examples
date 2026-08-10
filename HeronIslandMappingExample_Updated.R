#### Mapping Example
#### Updated for current versions of R and packages :)

# ------------------------------------------------------------
# 1. Install packages
# ------------------------------------------------------------

# Only run this ONCE if packages are not installed
install.packages(c(
  "ggplot2",
  "ggmap",
  "ggspatial",
  "sf"
))


# ------------------------------------------------------------
# 2. Load libraries
# ------------------------------------------------------------

library(ggplot2)
library(ggmap)
library(ggspatial)
library(sf)


# ------------------------------------------------------------
# 3. Set working directory
# ------------------------------------------------------------

setwd("~/Desktop/mapping_R_examples") ### make this for your own computer/folder


# ------------------------------------------------------------
# 4. Load practice dataset - Heron Island
# ------------------------------------------------------------

stations <- read.csv(
  "station_gps.csv",
  header = TRUE
)


# Look at data
head(stations)


# ------------------------------------------------------------
# 5. Register Google Maps API key
# ------------------------------------------------------------

# Replace the text below with their own API key.

register_google(
  key = "PUT YOUR OWN CODE HERE!"
)


# ------------------------------------------------------------
# 6. Create station identifiers
# ------------------------------------------------------------

stations$station <- c(
  "01", "02", "03", "04", "05",
  "06", "07", "08", "09", "10"
)


# ------------------------------------------------------------
# 7. Classify stations by reef zone
# ------------------------------------------------------------

stations$Zone <- c(
  "inshore",
  "outer",
  "inshore",
  "mid",
  "outer",
  "inshore",
  "mid",
  "outer",
  "mid",
  "outer"
)


# ------------------------------------------------------------
# 8. Find center of study area
# ------------------------------------------------------------

map_center <- c(
  lon = mean(stations$Longitude),
  lat = mean(stations$Latitude)
)

map_center


# ------------------------------------------------------------
# 9. Download satellite map
# ------------------------------------------------------------

stationmap <- get_googlemap(
  center = map_center,
  zoom = 17,
  scale = 2,
  maptype = "satellite"
)


# ------------------------------------------------------------
# 10. Make map
# ------------------------------------------------------------

reefflatmap <- ggmap(stationmap) +
  
  geom_point(
    data = stations,
    aes(
      x = Longitude,
      y = Latitude,
      color = Zone
    ),
    size = 9
  ) +
  
  geom_text(
    data = stations,
    aes(
      x = Longitude,
      y = Latitude,
      label = station
    ),
    color = "white",
    size = 4.5
  ) +
  
  # Scale bar
  # ggspatial::annotation_scale(
  #   location = "bl",
  #   width_hint = 0.45,     
  #   pad_x = grid::unit(0.25, "in"),
  #   pad_y = grid::unit(0.25, "in"),
  # ) +
  
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.5,
    pad_x = grid::unit(0.25, "in"),
    pad_y = grid::unit(0.35, "in"),
    style = "ticks", ##or you can use "bar" - but this doesnt look as good for this current map
    line_col = "white",
    text_col = "white",
    line_width = 1.2,
    text_cex = 1.2

  )+
  # North arrow
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = grid::unit(0.25, "in"),
    pad_y = grid::unit(0.25, "in"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  
  # This makes sure your map is in the correct coordinate system 
  coord_sf(crs = 4326) +
  
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Zone"
  ) +
  
  theme(
    axis.text.x = element_text(
      size = 15,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20)
  )

reefflatmap


##save has a high res png file
png("reefflatmap.png", width = 7.0, height = 5.5, units = 'in', res = 600)
reefflatmap
dev.off()


