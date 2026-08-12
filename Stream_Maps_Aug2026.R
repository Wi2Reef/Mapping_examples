#### Pigeon River Map
#### Sheboygan County, Wisconsin

# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(sf)                 # Work with spatial vector data
library(ggplot2)            # Make maps and figures
library(ggspatial)          # Add scale bar and north arrow
library(rnaturalearth)      # Download Lake Michigan polygon
library(rnaturalearthdata)


# ------------------------------------------------------------
# 2. Download Wisconsin DNR stream data
# ------------------------------------------------------------

# Wisconsin DNR 24K Rivers and Streams ArcGIS service
# This dataset contains detailed stream lines and stream names.

stream_url <- paste0(
  "https://dnrmaps.wi.gov/arcgis/rest/services/",
  "ER_Biotics/ER_Biotics_WGS84_Hydro/MapServer/1/query?",
  "where=1%3D1",
  "&outFields=*",
  "&returnGeometry=true",
  "&f=geojson"
)

wi_streams <- st_read(stream_url)


# ------------------------------------------------------------
# 3. Crop streams to the Pigeon River study area
# ------------------------------------------------------------

# The full WDNR dataset contains streams from across Wisconsin.
# Crop to the Sheboygan area to make the dataset easier to work with.

sheboygan_streams <- st_crop(
  wi_streams,
  xmin = -88.5,
  xmax = -87.45,
  ymin = 43.50,
  ymax = 43.95
)


# ------------------------------------------------------------
# 4. Identify Pigeon River
# ------------------------------------------------------------

# Search both stream-name fields because WDNR may store names
# differently among stream segments.

pigeon_sheb <- sheboygan_streams[
  grepl(
    "Pigeon",
    sheboygan_streams$RIVER_SYS_NAME,
    ignore.case = TRUE
  ) |
    grepl(
      "Pigeon",
      sheboygan_streams$ROW_NAME,
      ignore.case = TRUE
    ),
]


# ------------------------------------------------------------
# 5. Define the map extent
# ------------------------------------------------------------

# Find the geographic extent of Pigeon River.

pigeon_bbox <- st_bbox(pigeon_sheb)

# Add a small amount of space around the stream so it is not
# plotted directly against the edges of the figure.

pigeon_xlim <- c(
  pigeon_bbox["xmin"] - 0.03,
  pigeon_bbox["xmax"] + 0.03
)

pigeon_ylim <- c(
  pigeon_bbox["ymin"] - 0.03,
  pigeon_bbox["ymax"] + 0.03
)


# ------------------------------------------------------------
# 6. Find tributaries that directly connect to Pigeon River
# ------------------------------------------------------------

# st_intersects() determines which stream segments physically
# touch or intersect Pigeon River.

touches_pigeon <- lengths(
  st_intersects(
    sheboygan_streams,
    pigeon_sheb
  )
) > 0

# Keep those directly connected stream segments.

pigeon_direct <- sheboygan_streams[
  touches_pigeon,
]


# ------------------------------------------------------------
# 7. Identify directly connected unnamed tributaries
# ------------------------------------------------------------

# Keep only streams classified as "Unnamed".
# These represent unnamed tributaries that enter Pigeon River.

direct_unnamed <- pigeon_direct[
  pigeon_direct$RIVER_SYS_NAME == "Unnamed" &
    pigeon_direct$ROW_NAME == "Unnamed",
]


# Each Wisconsin waterbody has a unique WBIC number.
# Extract the WBIC numbers for unnamed tributaries that directly
# connect to Pigeon River.

tributary_wbics <- unique(
  direct_unnamed$RIVER_SYS_WBIC
)


# Now select ALL stream segments belonging to those tributaries.
# This prevents a tributary from turning gray partway upstream
# simply because WDNR divided it into multiple line segments.

pigeon_tribs <- sheboygan_streams[
  sheboygan_streams$RIVER_SYS_WBIC %in% tributary_wbics,
]


# ------------------------------------------------------------
# 8. Identify the Pigeon River mainstem
# ------------------------------------------------------------

# WBIC 62300 corresponds to Pigeon River in this dataset.

pigeon_main <- sheboygan_streams[
  sheboygan_streams$RIVER_SYS_WBIC == 62300,
]


# ------------------------------------------------------------
# 9. Download Lake Michigan polygon
# ------------------------------------------------------------

# Natural Earth provides polygons for major lakes.

lakes <- ne_download(
  scale = 10,
  type = "lakes",
  category = "physical",
  returnclass = "sf"
)

# Keep only Lake Michigan.

lake_michigan <- lakes[
  lakes$name == "Lake Michigan",
]


# ------------------------------------------------------------
# 10. Make final Pigeon River map
# ------------------------------------------------------------

pigeon_map <- ggplot() +
  
  # Plot all surrounding streams in light gray for context.
  geom_sf(
    data = sheboygan_streams,
    color = "grey75",
    linewidth = 0.25
  ) +
  
  # # Highlight unnamed tributaries that directly connect
  # # to Pigeon River.
  # geom_sf(
  #   data = pigeon_tribs,
  #   color = "dodgerblue3",
  #   linewidth = 0.8
  # ) +
  
  # Plot Pigeon River slightly thicker than its tributaries.
  geom_sf(
    data = pigeon_main,
    color = "dodgerblue3",
    linewidth = 1.3
  ) +
  
  # Add Lake Michigan as a gray polygon.
  geom_sf(
    data = lake_michigan,
    fill = "grey80",
    color = "grey60",
    linewidth = 0.4
  ) +
  
  # Zoom to the Pigeon River study area.
  coord_sf(
    xlim = pigeon_xlim,
    ylim = pigeon_ylim,
    expand = FALSE
  ) +
  #add survey points from SurveySites with labels for Site
  geom_text(data = SurveySites, aes(x = Longitude, 
                                    y = Latitude, 
                                    label = Site), 
            color = "black", size = 3, vjust = -1) +
  
  geom_point(data = SurveySites, aes(x = Longitude, 
                                     y = Latitude), color = "red", size = 3) +
  
  # Add stream name.
  annotate(
    geom = "text",
    x = -87.90,
    y = 43.93,
    label = "Pigeon River",
    fontface = "bold",
    color = "black",
    size = 9
  ) +
  
  # Label Lake Michigan.
  annotate(
    geom = "text",
    x = -87.71,
    y = 43.85,
    label = "Lake Michigan",
    fontface = "italic",
    color = "grey40",
    size = 5
  ) +
  
  # Add scale bar.
  annotation_scale(
    location = "bl",
    width_hint = 0.5
  ) +
  
  # Add north arrow.
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # Axis labels.
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Clean figure formatting.
  theme_classic(base_size = 15)


# View final map
pigeon_map

#### Silver Creek Map
#### Manitowoc County, Wisconsin

# ------------------------------------------------------------
# 11. Crop streams to the Silver Creek study area
# ------------------------------------------------------------

# Crop the statewide WDNR stream dataset to the Manitowoc area.

manitowoc_streams <- st_crop(
  wi_streams,
  xmin = -88.1,
  xmax = -87.45,
  ymin = 43.85,
  ymax = 44.15
)


# ------------------------------------------------------------
# 12. Identify Silver Creek
# ------------------------------------------------------------

# Search both stream-name fields because WDNR may store
# stream names differently among individual stream segments.

silver_man <- manitowoc_streams[
  grepl(
    "Silver",
    manitowoc_streams$RIVER_SYS_NAME,
    ignore.case = TRUE
  ) |
    grepl(
      "Silver",
      manitowoc_streams$ROW_NAME,
      ignore.case = TRUE
    ),
]


# ------------------------------------------------------------
# 13. Define the map extent
# ------------------------------------------------------------

# Find the geographic extent of Silver Creek.

silver_bbox <- st_bbox(silver_man)

# Add a small amount of space around the stream.

silver_xlim <- c(
  silver_bbox["xmin"] - 0.03,
  silver_bbox["xmax"] + 0.03
)

silver_ylim <- c(
  silver_bbox["ymin"] - 0.03,
  silver_bbox["ymax"] + 0.03
)


# ------------------------------------------------------------
# 14. Find tributaries that directly connect to Silver Creek
# ------------------------------------------------------------

# Determine which stream segments physically touch
# or intersect Silver Creek.

touches_silver <- lengths(
  st_intersects(
    manitowoc_streams,
    silver_man
  )
) > 0

# Keep directly connected stream segments.

silver_direct <- manitowoc_streams[
  touches_silver,
]


# ------------------------------------------------------------
# 15. Identify directly connected unnamed tributaries
# ------------------------------------------------------------

# Keep only connected stream segments classified as "Unnamed".

direct_unnamed_silver <- silver_direct[
  silver_direct$RIVER_SYS_NAME == "Unnamed" &
    silver_direct$ROW_NAME == "Unnamed",
]


# Extract their unique Wisconsin Waterbody Identification
# Code (WBIC) numbers.

silver_tributary_wbics <- unique(
  direct_unnamed_silver$RIVER_SYS_WBIC
)


# Select ALL segments belonging to those tributaries.

silver_tribs <- manitowoc_streams[
  manitowoc_streams$RIVER_SYS_WBIC %in%
    silver_tributary_wbics,
]


# ------------------------------------------------------------
# 16. Identify the Silver Creek mainstem
# ------------------------------------------------------------

# First inspect the WBIC number(s) associated with Silver Creek.

unique(
  silver_man[, c(
    "RIVER_SYS_NAME",
    "ROW_NAME",
    "RIVER_SYS_WBIC"
  )]
)

# Replace XXXXX with the correct Silver Creek WBIC.

silver_main <- manitowoc_streams[
  manitowoc_streams$RIVER_SYS_WBIC == "Silver Creek",
]


# ------------------------------------------------------------
# 17. Make final Silver Creek map
# ------------------------------------------------------------

silver_map <- ggplot() +
  
  # # Lake Michigan background.
  # geom_sf(
  #   data = lake_michigan,
  #   fill = "grey75",
  #   color = "grey60",
  #   linewidth = 0.4
  # ) +
  
  # Plot all surrounding streams in light gray.
  geom_sf(
    data = manitowoc_streams,
    color = "grey75",
    linewidth = 0.25
  ) +
  
  # # Highlight unnamed tributaries that directly connect
  # # to Silver Creek.
  # geom_sf(
  #   data = silver_tribs,
  #   color = "dodgerblue3",
  #   linewidth = 0.8
  # ) +
  
  # Plot Silver Creek slightly thicker.
  geom_sf(
    data = silver_man,
    color = "dodgerblue3",
    linewidth = 1.3
  ) +
  
#add survey points from SurveySites with labels for Site
  geom_text(data = SurveySites, aes(x = Longitude, 
                                   y = Latitude, 
                                   label = Site), 
            color = "black", size = 3, vjust = -1) +
  
  geom_point(data = SurveySites, aes(x = Longitude, 
                                     y = Latitude), color = "red", size = 3) +
  
  # Zoom to the Silver Creek study area.
  coord_sf(
    xlim = silver_xlim,
    ylim = silver_ylim,
    expand = FALSE
  ) +
  
  
  # Add stream name.
  annotate(
    geom = "text",
    x = -87.825,
    y = 44.11,
    label = "Silver Creek",
    fontface = "bold",
    color = "black",
    size = 6
  ) +
  
  # Label Lake Michigan.
  annotate(
    geom = "text",
    x = silver_xlim[2] - 0.03,
    y =silver_ylim[1] + 0.02,
    label = "Lake Michigan",
    fontface = "italic",
    color = "grey40",
    size = 5
  ) +
  
  # Add scale bar.
  annotation_scale(
    location = "bl",
    width_hint = 0.6
  ) +
  
  # Add north arrow.
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  
  # Axis labels.
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  # Clean map formatting.
  theme_classic(base_size = 15)


# View final map
silver_map

SurveySites<-read.csv("2026_GPSLocations.csv")
#rename column names as Latitude and Longitude
SurveySites<-SurveySites %>% rename(Latitude = Lat, Longitude = Lon)

