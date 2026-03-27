#grab EDDMapS data for noxious weeds in New Mexico

library(tidyverse)
library(readxl)
library(jsonlite)
library(sf)
library(janitor)
library(lubridate)
library(viridis)

# download all of the EDDMapS data for listed species

#grab a list of NM noxious weeds
list <- read_xlsx("scripts/dashboard/data/tables/Nox_weed_list.xlsx") %>% #"data/tables/Nox_weed_list.xlsx"
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

#shorten common name
nox_emd <- nox_emd %>%
  mutate(displayname = gsub(
    "\\s*\\((?:[^()]++|(?R))*\\)",   #recursive pattern
    "",
    displayname,
    perl = TRUE
  ))

#convert area values to sq. acres
nox_emd <- nox_emd %>% 
  mutate(
    # normalise unit strings once
    unit_norm = str_to_lower(infestedareaunits),
    
    # convert every row to acres
    infestedarea_acres = case_when(
      str_detect(unit_norm, "acre")             ~ infestedarea,                       # already acres
      str_detect(unit_norm, "meter")            ~ infestedarea / 4046.8564224,        # m² → acres
      str_detect(unit_norm, "foot|feet|ft")     ~ infestedarea / 43560,               # ft² → acres
      TRUE                                      ~ NA_real_                            # unknown or missing
    ),
    infestedarea_acres = as.numeric(format(infestedarea_acres, scientific = FALSE)) #remove scientific notation
  )%>% 
  select(-unit_norm)     # drop the temporary column (optional)

##### manually shorten the names
nox_emd <- nox_emd %>%
  clean_names() %>%  # standardize all names to snake_case
  rename(
    sci_name   = scientificname,
    disp_name  = displayname,
    subj_num   = subjectnumber,
    local_own  = local_ownership,
    coords     = coordinates,
    coord_unc  = coordinateuncertaintyinmeters,
    waterbody  = waterbodyname,
    num_coll   = numbercollected,
    area_gross = grossarea,
    units_gros = grossareaunits,
    area_inf   = infestedarea,
    units_inf  = infestedareaunits,
    acres_inf  = infestedarea_acres,
    area_treat = treatmentarea,
    treat_comm = treatmentcomments,
    disturb    = disturbance,
    qty_appx   = appxquantity,
    pct_cover  = percentcover,
    date_erad  = eradicationdate,
    plants_trt = plantstreated,
    stat_infst = infestationstatus,
    stat_erad  = eradicationstatus,
    date_obs   = observationdate,
    yr_acc     = yearaccuracy,
    date_ent   = dateentered,
    date_upd   = dateupdated,
    upd_by     = updatedby,
    surv_id    = surveyorid,
    obs_id     = observationidentifier,
    date_unc   = dateuncertaintyindays,
    subj_orig  = originalreportedsubject,
    host_phen  = hostphenology,
    unit_incd  = incidenceunit,
    str_incd   = incidencestring,
    unit_sev   = severityunit,
    str_sev    = severitystring,
    verify     = verificationmethod,
    cred       = identificationcredibility,
    rev_id     = reviewidentifier,
    date_ident = identificationdate,
    date_revd  = datereviewed,
    comm_pub   = publiccomments,
    method     = datacollectionmethod,
    oth_srv    = othersurveyors,
    rptr_affil = reporteraffiliation,
    proj       = projectname,
    unit_qty   = quantityunits,
    trap       = traptype,
    traps      = numberoftraps,
    tgt_ct     = targetcount,
    tgt_range  = targetrange,
    geogtype   = geogtype,
    rec_basis  = recordbasis,
    stat_mgmt  = managementstatus,
    stat_pop   = populationstatus,
    centroid   = centroidtype,
    rec_id     = original_record_id,
    map_id     = mappinguuid,
    resist     = resistanceprofiles,
    area_acres = infestedarea_acres
  )

# provide a color value for each species
u_sps <- tibble(disp_name = unique(nox_emd$disp_name))  #generate list of unique species
  
colors <- viridis::turbo(count(unique(u_sps))) #create colors for each unique sp.

sp_colors <- u_sps %>% #bind the two columsn together
  cbind(colors)

#join the color data to the nox weed data
nox_emd <- nox_emd %>%
  left_join(sp_colors, by = "disp_name")

#define column data type
nox_emd <- nox_emd %>%
  mutate(fipscode = as.integer(fipscode),
         num_coll = as.integer(num_coll),
         area_gross = as.numeric(area_gross),
         area_inf = as.numeric(area_inf),
         area_treat = as.numeric(area_treat),
         ptc_cover = as.numeric(pct_cover),
         date_erad = as_date(ymd_hms(as.character(date_erad), tz="UTC")),
         plants_trt = as.integer(plants_trt),
         date_obs = as_date(ymd_hms(as.character(date_obs), tz="UTC")),
         yr_acc = as.numeric(yr_acc),
         date_ent = as_date(ymd_hms(as.character(date_ent), tz="UTC")),
         date_upd = as_date(ymd_hms(as.character(date_upd), tz="UTC")),
         date_unc = as_date(ymd_hms(as.character(date_unc), tz="UTC")),
         date_ident = as_date(ymd_hms(as.character(date_ident), tz="UTC")),
         date_revd = as_date(ymd_hms(as.character(date_revd), tz="UTC")), 
         quantity = as.integer(quantity),
         tgt_ct = as.integer(tgt_ct),
         area_acres = as.numeric(area_acres))

#####separate data by geotype (point, line (non currently available), polygon, multipolygon) and save

##points data
points_shp <- nox_emd %>%
  filter(geogtype=="Point") %>%
  mutate(geometry = st_as_sfc(geogwkt, crs = 4326)) %>% # Add a spatial geometry column
  st_as_sf()

#write out csv
#write.csv(nox_emd, paste0("EDDMapS/EDDMapS_raw/nox_emd_raw_pts.csv")) 
  
#save shapefile - fields will be abbreviated
st_write(points_shp, 
         dsn = paste0("C:/Users/dburruss/Documents/GIS/EDDMapS/EDDMapS_raw/points_raw.shp"),
         driver = "ESRI Shapefile",
         append = FALSE)  # Overwrite if the file already exists
         
  
##Polygon
polygon_shp <- nox_emd %>%
  filter(geogtype=="Polygon") %>%
  mutate(geometry = st_as_sfc(geogwkt, crs = 4326)) %>% # Add a spatial geometry column
  st_as_sf() 

##MultiPolygon
multipolygon_shp <- nox_emd %>%
  filter(geogtype=="MultiPolygon") %>%
  mutate(geometry = st_as_sfc(geogwkt, crs = 4326)) %>% # Add a spatial geometry column
  st_as_sf() 

#Combine polygon and multipolygon files
poly_shp <- rbind(polygon_shp, multipolygon_shp)

st_write(poly_shp,
         dsn = paste0("C:/Users/dburruss/Documents/GIS/EDDMapS/EDDMapS_raw/polygon_raw.shp"),
         driver = "ESRI Shapefile",
         append = FALSE)  # Overwrite if the file already exists


# #####plot data for sanity check
# Download New Mexico state boundaries
# states <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>% 
#   filter(ID %in% c("new mexico", "arizona", "texas", "colorado"))
# 
# # Load county boundaries for visualization
# counties <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
# 
# #ggplot map 
# ggplot() +
#   geom_sf(data = counties, fill = NA, color = "gray50", size = 0.25) +
#   geom_sf(data = states, fill = NA, color = alpha("red",0.25), linewidth = 1.5) + 
#   
#   geom_sf(data = points_shp, aes(color = as.factor(sci_name)), shape = 16, size = 2) +
#   geom_sf(data = poly_shp, aes(fill = as.factor(sci_name), color = as.factor(sci_name)), alpha = 0.2) +
#   
#   scale_color_viridis(discrete = TRUE, option = "turbo") +
#   scale_fill_viridis(discrete = TRUE, option = "turbo") +
#   
#   # Customize plot appearance
#   labs(title = "Noxious Weeds in New Mexico",
#        x = "Longitude", 
#        y = "Latitude") +
#   theme_classic() +  # Use a minimal theme for the plot
#   theme(legend.position = "none",) +  # Remove the legend if you don't need it
#   coord_sf(xlim = c(-109.05, -103), ylim = c(31.4, 37.0))  # Set the coordinate limits for New Mexico
# 
# ggsave("C:/Users/dburruss/Documents/GIS/EDDMapS/Noxious_weeds_map.jpg")

