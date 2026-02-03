#load packages
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(readxl)
library(stringr)


##Set this filepath to the folder containing the shapefile
setwd("C:/Users/dburruss/Documents/GIS/HSP/HSP_project_original_shapefiles/Healthy_Soil_Program_WFL1")

##read in the existing shapefile
shapefile <- vect("HSP_Projects.shp") 

plot(shapefile) #map of plots
names(shapefile) #column headings in shapefile

##read in the table of new inforamtion
new_data <- read_xlsx("HSP GIS Project Data - FY 20-24.xlsx",
                      sheet = "FY24",
                      skip = 1) %>% # Header information in row 2
 as_tibble() %>%
 select(-`Project Type`, #remove columns not occurring in shapefile [-column name]
        -`Project Year...18`,
        -`Practices...22`,
        -`...12`,
        -`...16`,
        -`...23`,
        -`...30`,
        -`County...20`,
        -`Project Land Type...21`) %>%
  rename("Eligible Entity"=`Project Sponsor or Eligible Entity`,
         "Acres"="Project Size (acres)") %>%
  rename_with(~ substr(., 1,10)) %>% #shorten name to 10 characters
  rename_with(~ gsub("[.0-9]", "", .)) %>% #remove "." and digits 
  rename_with(~ gsub(" ", "_", .)) %>% #replace space with "_" for ESRI
  mutate(CreationDa= "2024/08/22", #add columns and information to match original shapefile
         Creator= "NMDA_ndb",
         EditDate= "2024/08/22",
         Editor= "NMDA_ndb",
         x= as.numeric(str_extract(Location_f, "(?<=,).*")),
         y= as.numeric(str_extract(Location_f,"^[^,]+"))) %>%
  select(-Location_f) %>%
  rename("Zip_Code"="ZIP_Code",
        "No_till_co"="No-till/co",
        "Multi_crop"="Multi-crop",
        "Crop_lives"="Crop-lives",
        "Wetland_ri"="Wetland/ri") %>%
  mutate(across(Cropland:Other, ~ ifelse(. == "X","Y"))) %>%
  mutate(across(Cropland:Other, ~ replace(., is.na(.),"N"))) %>%
  mutate(Project_Ye = as.character(Project_Ye),
         Acres = as.numeric(Acres))

#names(new_data) #columns names in new data

##convert the data to a shapefile
new_shape <- new_data %>%
  vect(geom=c("x","y"), crs="EPSG:4326") #convert table to shapefile


new_shape <- project(new_shape, crs(shapefile, proj=TRUE)) #reproject to match shapefile

plot(shapefile) #plot original shapefile
plot(new_shape, #plot new points
     col='cyan',
     add=TRUE)

## Append shapefiles and save
New_Shapefile <- rbind(shapefile, new_shape)

## Correct errors in the shapefile
New_Shapefile <- New_Shapefile %>%
  mutate(Grantee_Ty = if_else(Grantee_Ty == "Eligibility Entity", "Eligible Entity",Grantee_Ty)) %>%
  mutate(across(everything(), ~ str_replace_all(., "Cuidad","Ciudad"))) %>%
  mutate(across(everything(), ~ str_replace_all(., "San Juan  SWCD","San Juan SWCD"))) %>%
  mutate(across(everything(), ~ str_replace_all(., "DeBaca","De Baca"))) 
 

#unique(New_Shapefile$Project_Sp[str_starts(New_Shapefile$Project_Sp, "De")])

look <- New_Shapefile %>% as_tibble()
write.csv(look,"C:/Users/dburruss/Documents/HSP/ESRI_online_map/new_data.csv")

#look

writeVector(New_Shapefile,"C:/Users/dburruss/Documents/HSP/ESRI_online_map/HSP_project_shapefile/HSP_Projects.shp", overwrite=TRUE)
