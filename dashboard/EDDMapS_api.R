#grab EDDMapS data for noxious weeds in New Mexico

library(tidyverse)
library(readxl)
library(jsonlite)
library(sf)

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
nox_emd <- nox_emd %>%
  filter(!is.na(coordinates)) %>%
  select(-nextpage, -previouspage, -page, -totalrows) %>%
  select(-all_of(list_columns))
  

#####plot data
# Download New Mexico state boundaries
states <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>% #grabs a map of US states
  filter(ID %in% c("new mexico","arizona","texas","colorado")) # filters the polygon to include only rows where column ID == 'new mexico'

counties <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))

#extract x and y values for plots
coords <- strsplit(nox_emd$coordinates, ",\\s*")
nox_emd$latitude <- as.numeric(sapply(coords, `[`, 1))
nox_emd$longitude <- as.numeric(sapply(coords, `[`, 2))

# Now create the plot using the new latitude and longitude columns
plot(nox_emd$longitude, nox_emd$latitude, 
     col=as.factor(nox_emd$scientificname), 
     pch=20, cex=1, 
     xlab="Longitude", 
     ylab="Latitude", 
     main="Noxious Weeds in New Mexico",
     xlim=c(-109.05, -103),
     ylim=c(31.4, 37.0))

plot(counties, col=NA,
     border="gray50",
     lwd=0.25,
     add=TRUE)

plot(states, 
     col=NA, 
     border=adjustcolor("red", alpha.f = 0.25),  # Set alpha transparency level (0 = fully transparent, 1 = fully opaque)
     lwd=5, 
     add=TRUE)

#####separate data by geotype (point, line, polygon)

#points data
nox_emd %>%
  filter(geogtype=="Point") %>%
  write.csv(paste0("data/tables/EDDMapS_raw/nox_emd_raw_pts_",Sys.Date(),".csv"))
  
NEED TO SORT OUT POLYGON DATA ISSUES

#Polygons
test <- nox_emd %>%
  filter(geogtype=="MultiPolygons") %>%
  write.csv(paste0("data/tables/EDDMapS_raw/nox_emd_raw_pts_",Sys.Date(),".csv"))

test$geogwkt[1]

unique(nox_emd$geogtype)



#MultiPolygons

nox_emd_na <- nox_emd %>%
  filter(is.na(geogtype))



#write out raw data
write.csv(nox_emd,paste0("data/tables/EDDMapS_raw/nox_emd_data_",Sys.Date(),".csv"))

# #convert to a shapefile
# nox_emd_sf <- st_as_sf(nox_emd, coords = c("longitude", "latitude"), crs = 4326)

# #write shapefile out
# st_write(nox_emd_sf, "data/shapefiles/Processed_NW_data/EDDMapS_NM_NW.shp")

