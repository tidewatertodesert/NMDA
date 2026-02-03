#load packages
library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(readxl)
library(stringr)


##Set this filepath to the folder containing the shapefile
setwd("C:/Users/dburruss/Documents/GitHub/NMDA/")

##read in the existing shapefile
shapefile <- vect("C:/Users/dburruss/Documents/GIS/HSP/HSP_project_shapefile_2025_update/2024_Original_data/HSP_Projects_2024.shp") %>%
  select(-Field1)

names(shapefile)

##read in the table of new inforamtion
clean_data <- read_xlsx("HSP/ESRI_online_map/2025-2026_data/2025_clean_data/Updated_HSP_data_27Jan26.xlsx",
                        sheet = "Sheet1") %>%
  mutate(lat= as.numeric(lat),
         long= as.numeric(long)) %>%
  filter(!is.na(FY)) %>%
  mutate(X=long,
         Y=lat) %>%
  vect(geom = c("long", "lat"),
       crs = "EPSG:4326") %>%
  mutate(Project_Sp = Eligible_E) %>%
  select(Project_Ti,
         Grantee,
         Grantee_Ty,
         Project_Sp,
         County,
         Project_La,
         Amount_Spe,
         Practices,
         Project_Su,
         Town,
         Zip_Code,
         Cropland,
         Rangeland,
         Orchard,
         Subsistenc,
         Forest_Lan,
         Wildlife_H,
         Cover_crop,
         Planting_h,
         Establishi,
         No_till_co,
         Multi_crop,
         #Crop_lives, 
         Planned_gr, 
         Bale_grazi, 
         Mulching,   
         Compost_ap, 
         Microbial_, 
         #Wetland_ri, 
         Other,      
         Eligible_E, 
         Creator,    
         Editor,    
         X,          
         Y,          
         #FID_,       
         EditDate,   
         CreationDa, 
         Acres,      
         Project_Ye)

clean_data <- project(clean_data, crs(shapefile))

unique(clean_data$Grantee_Ty)

shape_new <- rbind(shapefile, clean_data)

# #remove shapefile data and join clean table data to shapefile
# shape_new <- shapefile %>%
#   select(Project_Ti) %>%
#   left_join(clean_data, by=c("Project_Ti"="Project_Ti")) %>%
#   filter(!is.na(Grantee_Ty)) %>%
#   mutate(Project_Ye = as.numeric(Project_Ye),
#          Acres = round(as.numeric(Acres), 2),
#          Amount_Spe = round(as.numeric(Amount_Spe), 2),
#          CreationDa = as.character(CreationDa),
#          EditDate = as.character(EditDate)) %>%
#   dplyr::select(-...2)
# 
# shape_new <- shape_new %>%
#   mutate(X = crds(shape_new)[,1],
#          Y = crds(shape_new)[,2],
#          FID = 1:nrow(shape_new))
# 
# names(shape_new)

plot(shape_new, col="red")

look <- shape_new %>% as_tibble()
write.csv(look,"HSP/ESRI_online_map/2025-2026_data/2025_clean_data/Updated_HSP_data_2020-2026.csv")

#write.csv(look %>% dplyr::select(FID, X, Y),"C:/Users/dburruss/Documents/HSP/ESRI_online_map/new_data_coords.csv")

#look

writeVector(shape_new,"C:/Users/dburruss/Documents/GIS/HSP/HSP_project_shapefile_2025_update/HSP_project_update_file/update_file_to_2026.shp", overwrite=TRUE)

# #convert to sf object to write longer fields
# shape_sf <- st_as_sf(shape_new)
# 
# #modify the character field type to handle longer strings
# shape_sf$Project_Su <- as.character(shape_sf$Project_Su)
# 
# #write to shapefile
# st_write(shape_sf, "C:/Users/dburruss/Documents/HSP/ESRI_online_map/HSP_project_shapefile_upload/HSP_Projects.shp", delete_dsn = TRUE)
