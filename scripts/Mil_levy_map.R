#create map of SWCD's with mil levy
library(tidyverse)
library(readxl)
library(sf)
library(tigris)
library(ggrepel)


##open files for map

#swcd mil levey data
mlevy <- read_excel("C:/Users/dburruss/Documents/GitHub/NMDA/data/tables/2025_Mil_levy.xlsx") %>%
  as_tibble() %>%
  filter(!is.na(`Referendum Rate`))

#shapefile of swcd
swcd <- st_read("data/shapefiles/nmswcd/nmswcd.shp") %>%
  st_make_valid() %>% #fix invalid geometries
  mutate(NAME = ifelse(NAME=="Edgewood", "Tri-County", NAME)) %>%
  left_join(mlevy, by = c("NAME" = "SWCD")) %>%
  mutate(has_rate = !is.na(`Referendum Rate`))

#grab NM county boundaries
nm_co <- counties(state = "NM", cb = TRUE) %>%  # cb = TRUE for simplified geometries
  st_transform(crs=st_crs(swcd)) %>%
  select(NAME, NAMELSAD) %>%
  rename(County = NAME,
         County_lsad = NAMELSAD)

# # Get centroids for labeling
# label_points <- st_centroid(swcd)

#produce plot
ggplot() +
  geom_sf(data = swcd, aes(fill = has_rate), color = "navajowhite", size = 0.05) +
  
  geom_sf(data = nm_co, fill = NA, color = "black", linetype = "dashed", size = 0.3, alpha=0.25) +
  
  geom_text_repel(
    data = label_points,
    aes(geometry = geometry, label = NAME),
    stat = "sf_coordinates",
    size = 3,
    color = "red4",
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  
  scale_fill_manual(
    values = c("FALSE" = "oldlace", "TRUE" = "brown2"),
    guide = "none"  # Remove legend
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA)
  )


#save plot
ggsave("C:/Users/dburruss/Documents/GitHub/NMDA/figures/Mil_levy_map_2025.jpg", width = 8, height = 10, dpi = 300)
