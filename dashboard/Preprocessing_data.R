#Process manually downloaded data for dashboard construction
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(stars)
library(tigris)
library(lubridate)

#Run this after the EDDMapS_api.R script
####grab the shapefiles

#point data
points <- st_read("EDDMapS/EDDMapS_raw/points_raw.shp")
message(nrow(points)," features in points layer.")
#names(points)

#simple polygons
sinpolyn <- st_read("EDDMapS/EDDMapS_raw/polygon_raw.shp") %>%
  st_make_valid()
message(nrow(sinpolyn)," features in sinpolyn layer.")
#names(sinpolyn)

#multipart polygons
mulpoly <- st_read("EDDMapS/EDDMapS_raw/multipolygon_raw.shp") %>%
  st_make_valid()
message(nrow(mulpoly)," features in mulpoly layer.")
#names(mulpoly)

#join polygons types for simpler processing
polygons <- rbind(sinpolyn, mulpoly)
message(nrow(polygons)," features in combined polygon layer")

message("Total records: ",nrow(points)+nrow(sinpolyn)+nrow(mulpoly))

#####select columns to keep from records
# columns <- c("scientificname",
#              "displayname",
#              "coordinates",
#              "geogtype",
#              "geogwkt",
#              "observationdate",
#              "infestationstatus",
#              "eradicationstatus",
#              "infestedarea",
#              "infestedareaunits",
#              "reporter")

columns <- c("scntfcn",
             "dsplynm",
             "cordnts",
             "geogtyp",
             "geogwkt",
             "obsrvtndn",
             "infsttn",
             "erdctns",
             "infestdr",
             "infstdrn",
             "reportr")


#select columns from point data
points_sl <- points %>%
  dplyr::select(columns)

#select columns from combined polygon data
polygons_sl <- polygons %>%
  select(columns)

#convert polygon centroids to points (dashboard summaries only use point data)
pt_cent <- polygons_sl %>%
  st_centroid()

#add centroids to points layer
points_fin <- points_sl %>%
  rbind(pt_cent)
nrow(points_fin)

####bring in state county and swcd data to add column to each shapefile
#read in shapefile of the SWCDs
SWCD <- st_read("data/shapefiles/nmswcd/nmswcd.shp") %>%
  st_make_valid(SWCD) %>% # fix invalid geometries
  mutate(NAME = ifelse(NAME=="Edgewood", "Tri-County", NAME)) %>%
  st_transform(crs=st_crs(points)) %>%
  select(NAME, Abbr) %>%
  rename(SWCD_name = NAME,
         SWCD_abbr = Abbr) 
  
#identical(st_crs(points), st_crs(SWCD)) #test if crs match

#grab NM county boundaries
nm_COUNTY <- counties(state = "NM", cb = TRUE) %>%  # cb = TRUE for simplified geometries
  st_transform(crs=st_crs(points)) %>%
  select(NAME, NAMELSAD) %>%
  rename(County = NAME,
         County_lsad = NAMELSAD)

#identical(st_crs(points), st_crs(nm_COUNTY)) #test if crs match
names(nm_COUNTY)

#### Compute the county and swcd intersect for points and polygon layers
points_join <- points %>% 
  st_join(nm_COUNTY) %>%
  st_join(SWCD) 

  #summary(points_join)  
  
polygon_int <- polygons %>%
  st_intersection(SWCD) %>%
  st_intersection(nm_COUNTY)

#change the format of columns to reflect data
points_join <- points_join %>%
  mutate(ObsDate = mdy(obsrvtndn),
         Year = year(ObsDate),
         Abundance = as.numeric(abundnc),
         InfestAcre = as.numeric(InfestAcre),
         GrossAcre = as.numeric(grossar),
         Percentcov = as.numeric(prcntcv),
         Density = as.numeric(density),
         Quantity = as.numeric(quantty),
         QuantityU = as.numeric(qnttynt))

polygon_int <- polygon_int %>%
  mutate(ObsDate = mdy(ObsDate),
         Year = year(ObsDate),
         Abundance = as.numeric(Abundance),
         InfestAcre = as.numeric(InfestAcre),
         GrossAcre = as.numeric(GrossAcre),
         Percentcov = as.numeric(Percentcov),
         Density = as.numeric(Density),
         Quantity = as.numeric(Quantity),
         QuantityU = as.numeric(QuantityU),
         APPXQuant = as.numeric(APPXQuant))

#Add polygon centroids to points and append to points dataframe (used for calculating fields in dashboard)
centrds <- polygon_int %>%
  st_point_on_surface()

#combine points
points_join <- points_join %>%
  rbind(centrds)

#read in NLCD data
nlcd <- rast("C:/Users/dburruss/Documents/GIS/nlcd_conus_2019_prj/nlcd_2019_prj.img")

#grab NLCD lookup table
nlcd_tab <- read.csv("C:/Users/dburruss/Documents/GIS/nlcd_conus_2019_prj/nlcd_lookup_table.csv") %>%
  as_tibble() %>%
  mutate(NLCD_Value = as.factor(NLCD_Value))

# Make sure both CRS are the same, if not transform
identical(crs(nlcd), st_crs(points_join))


points_ext <- terra::extract(nlcd, points_join) %>%
  cbind(points_join) 

points_join <- points_ext %>%
  left_join(nlcd_tab, by=c("Layer_1"="NLCD_Value"))

write.csv(points_join,"C:/Users/dburruss/Documents/Nox_Weeds/R/EDDMaps_data/Processed_NW_data/NLCD_points.csv")


#write out the data
st_write(points_join, "C:/Users/dburruss/Documents/Nox_Weeds/R/EDDMaps_data/Processed_NW_data/EDDMapS_points.shp",
         layer = "EDDMapS_points",
         delete_dsn = TRUE) #geopackage used to retain longer field names

#write out the data
st_write(polygon_int, "C:/Users/dburruss/Documents/Nox_Weeds/R/EDDMaps_data/Processed_NW_data/EDDMapS_polygon.shp",
         layer = "EDDMapS_polygon")

#write out the data
st_write(nm_COUNTY, "C:/Users/dburruss/Documents/Nox_Weeds/R/EDDMaps_data/Processed_NW_data/NM_County.shp",
         layer = "NM_County")

#write out the data
st_write(SWCD, "C:/Users/dburruss/Documents/Nox_Weeds/R/EDDMaps_data/Processed_NW_data/SWCD.shp",
         layer = "SWCD") 
