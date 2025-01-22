#useful example: https://cran.r-project.org/web/packages/rinat/vignettes/rinat-intro.html

library(tidyverse)
library(rinat)
library(readxl)
library(httr)
library(sf)
library(tmap)
library(lubridate)

#setwd("C:/Users/dburruss/Documents/R/inaturalist/")

#define region of interests [approx. extent of NM]
bbox_new_mexico <- c(31.33216, -109.050431, 37.000233, -103.002043)

#####create a loop to download all of the iNaturalist data for listed species

#grab data from github and write to temp dir
GET("https://github.com/tidewatertodesert/iNaturalist_widget/blob/6fa3b3a5e4c34a4407e4044db9361635c8b75338/Nox_weed_list.xlsx", 
    write_disk("Nox_weed_list.xlsx", overwrite = TRUE))

list <- read_xlsx("C:/Users/dburruss/Documents/GitHub/NMDA/data/tables/Nox_weed_list.xlsx") %>%
  as_tibble() %>%
  mutate(sp_name = gsub(" spp\\.", "",paste0(Genus," ",species))) 

#create blank dataframe
nox_obs <- tibble()  # Initialize an empty tibble (data frame)

#loop to retrieve all noxious species data
for (i in list$sp_name) {
  message("Searching records for: ",i)
  tryCatch({ #use trycatch to prevent warnings from interrupting download
    obs <- get_inat_obs(taxon_name = as.character(i), bounds = bbox_new_mexico)
    nox_obs <- rbind(nox_obs, obs)
    message(nrow(obs)," records found for ",i)
    rm(obs)
  }, error = function(e) {
    cat("No data found for species:", i, "\n")
  })
}


#save raw data
write.csv(nox_obs, paste0("iNaturalist/data/inat_raw_data_",Sys.Date(),".csv"))

#convert data to shapefile, reduce fields, and save
inat_obs_sf <- nox_obs %>% #pipes this data through the next 3 lines of code
  select(longitude, latitude, datetime, common_name, scientific_name, quality_grade) %>% #selects specific vectors [columns of data]
  mutate(date = date(ymd_hms(datetime, tz="America/Denver"))) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) #define coordinate reference system
  
st_write(inat_obs_sf, dsn = paste0("iNaturalist/data/shapefile/inat_raw_data_",Sys.Date(),".shp"),
        driver = "ESRI Shapefile")

##### plot the data for a sanity check

# Download New Mexico state boundaries
usa <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>% #grabs a map of US states
  filter(ID=="new mexico") # filters the polygon to include only rows where column ID == 'new mexico'

# Create a basic plot 
ggplot(usa) + #uses a widely used plot function
  geom_sf(color = "#2b2b2b", fill = "white", size = 0.125) + #to plot a shapefile [in this case it is an 'sf' object so we use geom_sf]
  geom_sf(data=inat_obs_sf, aes(color=scientific_name, shape=quality_grade)) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) + #defines the coordinate reference system [crs]
  ggthemes::theme_map() + #provides many canned themes for plot [see https://ggplot2.tidyverse.org/reference/ggtheme.html for more]
  theme(legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), # Reduce legend key size
        legend.text = element_text(size = 7), # Reduce legend text size
        legend.box = "horizontal", # Arrange legend items horizontally
        legend.box.margin = margin(0, 0, 0, 0), # Adjust margin if needed
        legend.spacing.x = unit(0.1, 'cm')) # Adjust spacing between legend items) #modify plot components here [https://ggplot2.tidyverse.org/reference/theme.html]
