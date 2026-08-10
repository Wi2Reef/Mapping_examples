###Mapping example - if you need to indicate your study location on a larger scale

###set working directory


#run this if you dont have packages already installed
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   y"ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "ggthemes") 

###load libraries
library("ggplot2")
library("sf") 
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library("ggspatial")
library("ggthemes")
                 
world <- ne_countries(scale = "medium", returnclass = "sf")
                 
class(world)
                 
# ggplot(data = world) +
# geom_sf()
# ggplot(data = world) +
# geom_sf() +
                   
# World polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

# Study location
kbay <- data.frame(
  longitude = -157.815287,
  latitude = 21.461356
)

reef13 <- data.frame(longitude = c(-157.778), latitude = c(21.478)) 


usa <- subset(world, admin == "United States of America")
(mainland <- ggplot(data = usa) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 
                                                                       730000)))          
(hawaii  <- ggplot(data = usa) +
    geom_sf() +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 
                                                                23), expand = FALSE, datum = NA))
HI2<-  hawaii + geom_rect(xmin = -158.3, xmax = -157.6, ymin = 21.25, ymax = 21.75, 
                          fill = NA, colour = "black", size = 1.0) +
  annotate(geom = "text", x = -157.5, y = 22.6, label = "Hawaiian Islands", 
           fontface = "italic", color = "grey22", size = 4) +
  labs(x= " ", y= " ")

HI2<- HI2+  
  theme(panel.grid.major = element_line(colour = NULL, linetype = "NULL"), panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(fill = NA))
######      
HI2

#### Oahu Map
Oahu<-ggplot(data = world) +
  geom_sf() +
  labs(x= "longitude", y= "latitude")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", x = -158, y = 21.5, label = "Oʻahu", 
           fontface = "bold", color = "black", size = 9) +
  annotate(geom = "text", x = -157.7, y = 21.52, label = "K\u101neʻohe Bay", 
           fontface = "italic", color = "grey22", size = 7) +
  coord_sf(xlim = c(-158.275, -157.5), ylim = c(21.2, 21.75))
Oahu<-  Oahu  +
  theme(text = element_text(size=15), panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                                                    size = 0.5), panel.background = element_rect(fill = "white"), 
      panel.border = element_rect(fill = NA))

Oahu

library(gridExtra) #install these if needed
library(grid)
#inset maps
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = .9, height = 0.33, x = 0.8, y = 0.82) #plot area for the inset map
print(Oahu,vp=v1) 
print(HI2,vp=v2)

tiff("BIGFEAR_HI_MAP.tiff", width = 8, height = 5.5, units = 'in', res = 600)
print(Oahu,vp=v1) 
print(HI2,vp=v2)
dev.off()


###depending on your audience, you may need to indicate where your study site is on a global map...
##here is some code to do so:

globe <- ggplot(data = world) +
  geom_sf(fill = "grey90", color = "grey50", linewidth = 0.2) +
  
  # Hawaii location
  geom_point(
    data = kbay,
    aes(x = longitude, y = latitude),
    color = "red",
    size = 3
  ) +
  
  annotate(
    "text",
    x = -150,
    y = 27,
    label = "Hawaiʻi",
    fontface = "bold",
    size = 4
  ) +
  
  coord_sf(
    xlim = c(-180, 180),##maybe play with this and zoom in a bit on your study region
    ylim = c(-60, 85),###
    expand = FALSE
  ) +
  
  theme_void() +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.5
    )
  )

globe

