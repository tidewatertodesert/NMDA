#useful example: https://cran.r-project.org/web/packages/rinat/vignettes/rinat-intro.html

library(tidyverse)
library(rinat)
library(readxl)
library(httr)
library(sf)
library(tmap)
library(lubridate)

#define region of interests [approx. extent of NM]
bbox_new_mexico <- c(31.33216, -109.050431, 37.000233, -103.002043)

#####create a loop to download all of the iNaturalist data for listed species

#grab species list and make species name column
list <- read_xlsx("data/tables/Nox_weed_list.xlsx", sheet = "list") %>%
  as_tibble() %>%
  mutate(sp_name = gsub(" spp\\.", "",paste0(Genus," ",species))) 

#create blank dataframe
nox_obs <- tibble()  # Initialize an empty tibble (data frame)

#loop to retrieve all noxious species data
for (i in list$sp_name) {
  message("Searching records for: ",i)
  tryCatch({ #use trycatch to prevent warnings from interrupting download
    obs <- get_inat_obs(taxon_name = as.character(i), bounds = usa, maxresults = 10000)
    nox_obs <- rbind(nox_obs, obs)
    message(nrow(obs)," records found for ",i)
    rm(obs)
  }, error = function(e) {
    cat("No data found for species:", i, "\n")
  })
}


# Download New Mexico state boundaries for clipping data
NM <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>% #grabs a map of US states
  filter(ID=="new mexico") %>% # filters the polygon to include only rows where column ID == 'new mexico'
  st_transform(crs=4326)

#convert data to shapefile, keep only points within NM, and save
inat_obs_sf <- nox_obs %>% #pipes this data through the next 3 lines of code
  mutate(datetime = with_tz(ymd_hms(datetime), "America/Denver")) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% #define coordinate reference system
  st_intersection(NM) %>% # Keep only points inside the polygon
  mutate(longitude = st_coordinates(inat_obs_sf)[, 1],   #include longitude - first column
       latitude = st_coordinates(inat_obs_sf)[, 2])    #include latitude - second column

#save raw data
inat_obs_sf %>%
  #st_drop_geometry() %>%  
  write.csv("iNaturalist/data/inat_raw_data.csv")  

#select a subset of columns
inat_obs_sf <- inat_obs_sf %>%
  select(datetime, common_name, scientific_name, quality_grade) #selects specific vectors [columns of data]
  
#save shapefile
st_write(inat_obs_sf, dsn = "iNaturalist/data/shapefile/inat_raw_data_.shp",
        driver = "ESRI Shapefile",
        recursive = TRUE)



##### plot the data for a sanity check
# Create a basic plot 
ggplot(NM) +
  geom_sf(color = "#2b2b2b", fill = "white", size = 0.125) +
  geom_sf(data = inat_obs_sf, aes(color = scientific_name, shape = quality_grade)) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  ggthemes::theme_map() + #provides many canned themes for plot [see https://ggplot2.tidyverse.org/reference/ggtheme.html for more]
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),  
    legend.text = element_text(size = 6),  
    legend.box = "horizontal",
    legend.box.margin = margin(2, 2, 2, 2),
    legend.spacing.x = unit(0.1, 'cm')  # Adjust spacing between legend items) #modify plot components here [https://ggplot2.tidyverse.org/reference/theme.html]
  ) +
  guides(
    color = guide_legend(ncol = 3, override.aes = list(size = 3)),  # Split legend into 2 columns
    shape = guide_legend(ncol = 1)  # Keep shape legend separate if needed
  )

ggsave("iNaturalist/iNaturalist_map.jpg", width = 10, height = 8, dpi = 300)
