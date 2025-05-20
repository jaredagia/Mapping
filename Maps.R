rm(list = ls())
require(pacman)
pacman::p_load(ggplot2,ggmap,readxl,sf,tmap,dplyr,ggspatial)

# Load Nigeria Shapefile
nigeria <- st_read("Nigeria_Poly.shp")

# Plot Kano State
kano_nig <- nigeria %>% filter(grepl("Kano",wikimedia)) 

kano <- st_read("NGA_adm2.shp") %>% 
  filter(NAME_1 =="Kano")

#Keeping the administative areas.
kano_study <- kano %>% filter(grepl("Dala|Fagge|Gezawa|Gwale|Kano|Kumbotso|Nassaraw|Tarauni|Ungogo", NAME_2))

 # Calculate centroids for labeling
 nigeria$centroid <- st_centroid(nigeria$geometry)
 kano$centroid <- st_centroid(kano$geometry) 
 kano_study$centroid <- st_centroid(kano_study$geometry) 
 
 # Extract centroid coordinates
 nigeria_coords <- as.data.frame(st_coordinates(nigeria$centroid))
 nigeria_coords$state_name <- nigeria$name  

 kano_coords <- as.data.frame(st_coordinates(kano$centroid))
 kano_coords$NAME_2 <- kano$NAME_2  

 study_coords <- as.data.frame(st_coordinates(kano_study$centroid))
 study_coords$NAME_2 <- kano_study$NAME_2 
 
#Map of Nigeria Showing Kano
plot1 <- ggplot() +
  geom_sf(data = nigeria, fill = "lightyellow", color = "black") +  # Base map
  geom_sf(data = kano_nig, fill = "red", color = "black")+
  geom_text(data = nigeria_coords, aes(X, Y, label = state_name), size = 4, color = "black") + 
  coord_sf(datum = NA) + 
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey", linetype = "dashed"), # Customize grid lines
    panel.background = element_rect(fill = "lightyellow"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )  +
  annotate("text", x = Inf, y = Inf, label = "MAP OF NIGERIA SHOWING KANO STATE", 
           size = 4, color = "black", hjust = 1.6, vjust = 1.6)

ggsave("Nigeria.png", plot = plot1, width = 8, height = 6, dpi = 300)

#Map of Kano State Showing the Study Area
plot2 <- ggplot() +
  geom_sf(data = kano, fill = "lightyellow", color = "black") +  # Base map
  geom_sf(data = kano_study, fill = "red", color = "black") +
  geom_text(data = kano_coords, aes(X, Y, label = NAME_2), size = 3, color = "black") + 
  coord_sf(datum = NA) +  # Use coord_sf to add grid lines
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey", linetype = "dashed"), # Customize grid lines
    panel.background = element_rect(fill = "lightyellow"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  annotate("text", x = Inf, y = Inf, label = "MAP OF KANO STATE SHOWING THE STUDY AREA", 
           size = 4, color = "black", hjust = 1.1, vjust = 1.5)

ggsave("Kano.png", plot = plot2, width = 8, height = 6, dpi = 300)

#Map of study area
plot3 <- ggplot() +
  geom_sf(data = kano_study, fill = "red", color = "black") +  # Base map
  geom_text(data = study_coords, aes(X, Y, label = NAME_2), size = 4, color = "black") + 
  coord_sf(datum = NA) +  # Use coord_sf to add grid lines
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey", linetype = "dashed"), # Customize grid lines
    panel.background = element_rect(fill = "lightyellow"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  annotate("text", x = Inf, y = Inf, label = "STUDY AREA", 
           size = 5, color = "black", hjust = 4, vjust = 3)

ggsave("Study_Area.png", plot = plot3, width = 8, height = 6, dpi = 300)

