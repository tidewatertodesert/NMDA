library(tidyverse)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)  # Optional: higher-resolution boundaries
library(ggrepel)
library(stringr)

#set worsfheaders#set working directory
setwd("C:/Users/dburruss/Documents/GitHub/NMDA/")

#read in point data
pts <- st_read("LOE_project_maps/data/FY25_shapefile/LOE_FY25.shp", quiet = TRUE) %>%
  st_transform(pts, crs=st_crs(3857))


#get New Mexico geometry and transform it
nm <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "New Mexico") %>%
  st_transform(3857)  # Transform to Web Mercator (EPSG:3857)

#extract bounding box
nm_bbox <- st_bbox(nm)

#filter your points and plot for NW grant
ndg <- -19000
pts %>%
  filter(Grant == "Noxious Weed Management Grant") %>%
  st_transform(3857) %>%  # Transform points to match CRS of basemap
  ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(fill = "goldenrod1", color="black", size = 4, shape=21) +
  #geom_sf_text(aes(label=ID), size=3.5, nudge_x=ndg, nudge_y=0) + #geom_sf_tex
  #geom_sf_label(aes(label = ID), size=3.5, fill=NA) +
  geom_text_repel(
    aes(geometry = geometry, label = str_extract(ID, "(?<=-)[^-]+$")),
    stat = "sf_coordinates",       # tells ggrepel to use point coords
    seed = 1,                      # reproducible layout
    box.padding = 0.5,             # space around labels
    min.segment.length = 0,         # 'Inf' effectively disables segment - change value if desire
    fontface = "bold"
  ) +
  coord_sf(
    crs = 3857,
    xlim = c(nm_bbox["xmin"], nm_bbox["xmax"]),
    ylim = c(nm_bbox["ymin"], nm_bbox["ymax"])
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave("LOE_project_maps/NW_FY25_map.jpg", dpi=200, width = 6, height = 8, units = "in")

#filter your points and plot for DOG grant
ndg <- -19000
pts %>%
  filter(Grant == "District Opportunities Grant") %>%
  st_transform(3857) %>%  # Transform points to match CRS of basemap
  ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(fill = "deeppink", color="black", size = 4, shape=21) +
  #geom_sf_text(aes(label=ID), size=3.5, nudge_x=ndg, nudge_y=0) + #geom_sf_tex
  #geom_sf_label(aes(label = ID), size=3.5, fill=NA) +
  geom_text_repel(
    aes(geometry = geometry, label = str_extract(ID, "(?<=-)[^-]+$")),
    stat = "sf_coordinates",       # tells ggrepel to use point coords
    seed = 1,                      # reproducible layout
    box.padding = 0.5,             # space around labels
    min.segment.length = 0,         # 'Inf' effectively disables segment - change value if desire
    fontface = "bold"
  ) +
  coord_sf(
    crs = 3857,
    xlim = c(nm_bbox["xmin"], nm_bbox["xmax"]),
    ylim = c(nm_bbox["ymin"], nm_bbox["ymax"])
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave("LOE_project_maps/DOG_FY25_map.jpg", dpi=200, width = 6, height = 8, units = "in")

#filter your points and plot for HSPEE grant
ndg <- -19000
pts %>%
  filter(Grant == "Healthy Soil Program Eligible Entities") %>%
  st_transform(3857) %>%  # Transform points to match CRS of basemap
  ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(fill = "cornflowerblue", color="black", size = 4, shape=21) +
  #geom_sf_text(aes(label=ID), size=3.5, nudge_x=ndg, nudge_y=0) + #geom_sf_tex
  #geom_sf_label(aes(label = ID), size=3.5, fill=NA) +
  geom_text_repel(
    aes(geometry = geometry, label = str_extract(ID, "(?<=-)[^-]+$")),
    stat = "sf_coordinates",       # tells ggrepel to use point coords
    seed = 1,                      # reproducible layout
    box.padding = 0.5,             # space around labels
    min.segment.length = 0,         # 'Inf' effectively disables segment - change value if desire
    fontface = "bold"
  ) +
  coord_sf(
    crs = 3857,
    xlim = c(nm_bbox["xmin"], nm_bbox["xmax"]),
    ylim = c(nm_bbox["ymin"], nm_bbox["ymax"])
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave("LOE_project_maps/HSPEE_FY25_map.jpg", dpi=200, width = 6, height = 8, units = "in")

#filter your points and plot for HSPIA grant
ndg <- -19000
pts %>%
  filter(Grant == "Healthy Soil Program Individual Applicants") %>%
  st_transform(3857) %>%  # Transform points to match CRS of basemap
  ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0) +
  geom_sf(fill = "mediumspringgreen", color="black", size = 4, shape=21) +
  #geom_sf_text(aes(label=ID), size=3.5, nudge_x=ndg, nudge_y=0) + #geom_sf_tex
  #geom_sf_label(aes(label = ID), size=3.5, fill=NA) +
  geom_text_repel(
    aes(geometry = geometry, label = str_extract(ID, "(?<=-)[^-]+$")),
    stat = "sf_coordinates",       # tells ggrepel to use point coords
    seed = 1,                      # reproducible layout
    box.padding = 0.5,             # space around labels
    min.segment.length = 0,         # 'Inf' effectively disables segment - change value if desire
    fontface = "bold",
    max.overlaps = 25) +
  coord_sf(
    crs = 3857,
    xlim = c(nm_bbox["xmin"], nm_bbox["xmax"]),
    ylim = c(nm_bbox["ymin"], nm_bbox["ymax"])
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())

ggsave("LOE_project_maps/HSPIA_FY25_map.jpg", dpi=200, width = 6, height = 8, units = "in")

