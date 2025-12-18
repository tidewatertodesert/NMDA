#AWD map 

library(tigris)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(stringr)
library(shadowtext)

options(tigris_use_cache = TRUE)

# Load NM counties shapefile
nm_counties <- counties(state = "NM", cb = TRUE, class = "sf")

# Intern data
intern_data <- data.frame(
  county = c("Bernalillo", "Cibola", "Colfax", "Curry", "Doña Ana", 
             "Luna", "Otero", "Sandoval", "Santa Fe", "Socorro", 
             "Torrance", "Valencia"),
  interns = c(22, 2, 4, 3, 8, 1, 3, 11, 12, 3, 4, 2)
)

# Fix naming to match shapefile
intern_data <- intern_data %>%
  mutate(NAME = str_to_title(county))

# Join intern data with shapefile
nm_map <- nm_counties %>%
  left_join(intern_data, by = "NAME")

# Calculate county centroids for label placement
centroids <- nm_map %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame()

nm_map$label_x <- centroids$X
nm_map$label_y <- centroids$Y

# Plot
ggplot(nm_map) +
  geom_sf(aes(fill = interns), color = "white") +
  scale_fill_viridis(
    option = "viridis", 
    direction = -1,
    na.value = "grey90",
    name = "Interns"
  ) +
  # geom_text(
  #   aes(x = label_x, y = label_y, label = interns),
  #   size = 3,
  #   color = "black",
  #   na.rm = TRUE
  # ) +
  # Replace the geom_text() with geom_shadowtext()
  geom_shadowtext(
    aes(x = label_x, y = label_y, label = interns),
    size = 3,
    color = "black",
    bg.color = "white",   # White halo
    bg.r = 0.15,           # Radius of halo (adjust as needed)
    na.rm = TRUE
  )+
  labs(
    title = "Number of Interns by New Mexico County"#,
    # subtitle = "Data Visualized with Viridis Gradient",
    # caption = "Source: User Provided Data"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

ggsave("figures/Internship_map/Internships_x_County.jpg", width = 6, height = 7, units="in", dpi=200)
