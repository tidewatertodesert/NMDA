library(readxl)
library(tmap)
library(maps)
library(tidyverse)
library(tidyterra)
library(sf)

#set working directory
setwd("C:/Users/dburruss/Documents/GitHub/NMDA/")

#open table
df <- read_xlsx("LOE_project_maps/data/LOE_Report_Data_fy25-26prov.xlsx",
                sheet = "Report Tables") %>%
  as_tibble() %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long),
         corrected_lat = as.numeric(corrected_lat),
         corrected_long = as.numeric(corrected_long),

         # Locate_est = if_else(
         #   is.na(Notes) | str_trim(Notes) == "",   
         #   "No",                                  
         #   "Yes"                                  
         #   ),
         lat  = coalesce(corrected_lat, lat),  #overwrites data with corrected locates
         long = coalesce(corrected_long, long),
         jlat = jitter(lat, amount = 0.001),
         jlong = jitter(long, amount = 0.001),) # %>%
  # filter(loefund %in% c("n","y","q"))

points_sf <- st_as_sf(df, coords = c("jlong", "jlat"), crs = 4326)

points_dog <- points_sf %>%
  filter(Grant == "District Opportunities Grant")

points_hspee <- points_sf %>%
  filter(Grant == "Healthy Soil Program Eligible Entities")

points_hspia <- points_sf %>%
  filter(Grant == "Healthy Soil Program Individual Applicants")

points_nw <- points_sf %>%
  filter(Grant == "Noxious Weed Management Grant")

# # Bounding box: xmin, ymin, xmax, ymax
# nm_bbox <- sf::st_bbox(c(
#   xmin = -110,  # west of NM
#   xmax = -102,  # east of NM
#   ymin = 31,    # south of NM
#   ymax = 38     # north of NM
# ), crs = 4326)
# 
# # Convert bbox to polygon for plotting
# bbox_sf <- st_as_sfc(nm_bbox)

# Convert maps::map to sf and extract New Mexico
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
nm <- filter(states, ID == "new mexico")


# Use interactive map mode
tmap_mode("view")

my_map <- tm_basemap("OpenStreetMap") +
#   tm_shape(points_sf) +
#     tm_dots(size = 1, col = "Grant", palette = c("red","cyan","gold","chartreuse"), popup.vars = c("ID","Grant","entity","Title", "Funding","Address"), alpha=0.65)

  ## 1. points_dog  –– red
  tm_shape(points_dog) +
  tm_symbols(#shape = "Locate_est",
             shapes = c(Yes = 24, No = 21),
             col    = "red",
             size   = 1,
             border.col = "gray25",
             border.lwd = 0.1,
             alpha  = 0.65,
             popup.vars = c("ID","Grant","Entity","Title",
                            "Funding","Address"),
             legend.col.show   = FALSE,   # hide colour legend
             legend.shape.show = FALSE) + # hide shape legend (optional)
  
  ## 2. points_hspee  –– cyan
  tm_shape(points_hspee) +
  tm_symbols(#shape = "Locate_est",
             shapes = c(Yes = 24, No = 21),
             col    = "cyan",
             size   = 1,
             border.col = "gray25",
             border.lwd = 0.1,
             alpha  = 0.65,
             popup.vars = c("ID","Grant","Entity","Title",
                            "Funding","Address"),
             legend.col.show = FALSE,
             legend.shape.show = FALSE) +
  
  ## 3. points_hspia  –– gold
  tm_shape(points_hspia) +
  tm_symbols(#shape = "Locate_est",
             shapes = c(Yes = 24, No = 21),
             col    = "gold",
             size   = 1,
             border.col = "gray25",
             border.lwd = 0.1,
             alpha  = 0.65,
             popup.vars = c("ID","Grant","Entity","Title",
                            "Funding","Address"),
             legend.col.show = FALSE,
             legend.shape.show = FALSE) +
  
  ## 4. points_nw  –– chartreuse
  tm_shape(points_nw) +
  tm_symbols(#shape = "Locate_est",
             shapes = c(Yes = 24, No = 21),
             col    = "chartreuse",
             size   = 1,
             border.col = "gray25",
             border.lwd = 0.1,
             alpha  = 0.65,
             popup.vars = c("ID","Grant","Entity","Title",
                            "Funding","Address"),
             legend.col.show = FALSE,
             legend.shape.show = FALSE)

my_map

#save interactive map
tmap_save(my_map, filename = "LOE_project_maps/interactive_map.html")


points_cleaned <- points_sf %>%
  select(ID, FY, Grant, Funding, Title, Entity, Ent_Type, County, lat, long)
  
#save shapefile
st_write(points_cleaned, "C:/Users/dburruss/Documents/GIS/LOE_project_maps/shapefiles/LOE_FY25-26.shp", delete_layer = TRUE)

# #cleaned shapefile for LOE Coalition
# cleaned <- points_cleaned %>%
#   filter(!ID %in% c("HSPIA-FY25-061", "HSPIA-FY25-062", "HSPIA-FY25-073", "HSPIA-FY25-076", "HSPIA-FY25-077", "HSPIA-FY25-096", "HSPEE-FY25-024"))
#  
# st_write(cleaned, "LOE_project_maps/data/FY25_shapefile/LOE_FY25.shp", delete_layer = TRUE)


