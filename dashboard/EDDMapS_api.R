#grab EDDMapS data for noxious weeds in New Mexico

library(tidyverse)
library(readxl)
library(jsonlite)
library(sf)
library(viridis)

# download all of the EDDMapS data for listed species

#grab a list of NM noxious weeds
list <- read_xlsx("data/tables/Nox_weed_list.xlsx") %>%
  as_tibble() %>%
  mutate(sp_name = gsub(" spp\\.", "",paste0(Genus," ",species))) 

#create blank dataframe
nox_emd <- tibble()  # Initialize an empty tibble (data frame)


#loop to retrieve all noxious species data
for (i in list$EDDMapS_subnum) {
  message("Searching EDDMapS records for: ",i)
  
  #use trycatch to prevent warnings from interrupting download
  tryCatch({ 
    
    # Base URL for the API, with state, country, subjectid, and page # parameter
    # state and country require numeric value. NM = 35 and USA=926
    base_url <- paste0("https://api.bugwoodcloud.org/v2/occurrence?state=35&country=926&subjectid=",i,"&paging=true")

    json_data <- fromJSON(base_url)
    
    # get total observations and pages needed for each species i
    total_rows <- as.numeric(json_data$totalrows) #return the number of observations in data
    rows_per_request <- 50 # limit set by api
    total_pages <- ceiling(total_rows / rows_per_request) # calculate the total number of pages needed
    
    # initialize an empty data frame 
    all_results <- data.frame()
    
    #loop for ensure all data is downloaded
    if (total_rows >= rows_per_request) {
      
      #loop through request to grab all data 50 lines per request
      message("Records for species ",i," exceed API limit (50 obs.) and will loop through ", total_pages, " pages of data.")
      
      # Loop through each page and fetch data
      for (page in 1:total_pages) {
        # construct the URL for each page
        url <- paste0(base_url, "&page=",page)
        
        # fetch JSON data for the current page
        json_data <- fromJSON(url)
        
        # convert to data frame and append to all_results
        result <- as.data.frame(json_data)
        all_results <- rbind(all_results, result)
        message("Page ",page, " of ", total_pages, " downloaded")
      }
      message(total_rows, " records downloaded.")
      
    } else {
      # grab the observations without looping
      message("Downloaded ", total_rows, " records for species ",i)
      
      all_results <- as.data.frame(json_data)
      
    }
    
    nox_emd <- rbind(nox_emd, all_results)
    
  }, error = function(e) {
    cat("No records found for species:", i, "\n")
  })
  
}

##### some cleaning required for EDDMapS data
#remove the JSON structure from var names
colnames(nox_emd) <- gsub("^data\\.", "", colnames(nox_emd))

# Identify the column of class 'list'
column_classes <- sapply(nox_emd, class)
list_columns <- names(column_classes[column_classes == "list"])

#remove NA data !!!!!!!!!!!!!!!!!!!NEED TO EMAIL EDDMAPS THAT THIS IS OCCURRING. PROBABLE ISSUE WITH THEIR API
#remove vectors/columns of class 'list' for now
nox_emd <- nox_emd %>%
  filter(!is.na(coordinates)) %>%
  select(-nextpage, -previouspage, -page, -totalrows) %>%
  select(-all_of(list_columns))


#####separate data by geotype (point, line, polygon, multipolygon) and save

#points data
points_shp <- nox_emd %>%
  filter(geogtype=="Point") %>%
  mutate(geometry = st_as_sfc(geogwkt, crs = 4326)) %>% # Add a spatial geometry column
  st_as_sf()

#write out csv
#write.csv(nox_emd, paste0("EDDMapS/EDDMapS_raw/nox_emd_raw_pts.csv")) 
  
#save shapefile - fields will be abbreviated
st_write(points_shp, 
         dsn = paste0("EDDMapS/EDDMapS_raw/points_raw.shp"),
         driver = "ESRI Shapefile",
         append = FALSE)  # Overwrite if the file already exists
         
  
#Polygon
polygon_shp <- nox_emd %>%
  filter(geogtype=="Polygon") %>%
  mutate(geometry = st_as_sfc(geogwkt, crs = 4326)) %>% # Add a spatial geometry column
  st_as_sf() 
#write.csv(paste0("dashboard/data/tables/EDDMapS_raw/nox_emd_raw_pts.csv")) 

st_write(polygon_shp, 
         dsn = paste0("EDDMapS/EDDMapS_raw/polygon_raw.shp"),
         driver = "ESRI Shapefile",
         append = FALSE)  # Overwrite if the file already exists

#MultiPolygon
multipolygon_shp <- nox_emd %>%
  filter(geogtype=="MultiPolygon") %>%
  mutate(geometry = st_as_sfc(geogwkt, crs = 4326)) %>% # Add a spatial geometry column
  st_as_sf() 
#write.csv(paste0("dashboard/data/tables/EDDMapS_raw/nox_emd_raw_pts.csv")) 

st_write(multipolygon_shp, 
         dsn = paste0("EDDMapS/EDDMapS_raw/multipolygon_raw.shp"),
         driver = "ESRI Shapefile",
         append = FALSE)  # Overwrite if the file already exists


# #####plot data for sanity check
# Download New Mexico state boundaries
states <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>% 
  filter(ID %in% c("new mexico", "arizona", "texas", "colorado"))

# Load county boundaries for visualization
counties <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))

#ggplot map 
ggplot() +
  geom_sf(data = counties, fill = NA, color = "gray50", size = 0.25) +
  geom_sf(data = states, fill = NA, color = alpha("red",0.25), linewidth = 1.5) + 
  
  geom_sf(data = points_shp, aes(color = as.factor(scientificname)), shape = 16, size = 2) +
  geom_sf(data = polygon_shp, aes(fill = as.factor(scientificname), color = as.factor(scientificname)), alpha = 0.2) +
  geom_sf(data = multipolygon_shp, aes(fill = as.factor(scientificname), color = as.factor(scientificname)), alpha = 0.2) + 
  
  scale_color_viridis(discrete = TRUE, option = "turbo") +
  scale_fill_viridis(discrete = TRUE, option = "turbo") +
  
  # Customize plot appearance
  labs(title = "Noxious Weeds in New Mexico",
       x = "Longitude", 
       y = "Latitude") +
  theme_classic() +  # Use a minimal theme for the plot
  theme(legend.position = "none",) +  # Remove the legend if you don't need it
  coord_sf(xlim = c(-109.05, -103), ylim = c(31.4, 37.0))  # Set the coordinate limits for New Mexico

ggsave("EDDMapS/Noxious_weeds_map.jpg")
