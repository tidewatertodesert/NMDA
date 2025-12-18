library(sf)
library(tidyverse)
library(lubridate)
library(soilDB)
library(ggtern)
library(plotly)


setwd("C:/Users/dburruss/Documents/GitHub/NMDA")

#read in the shapefile of spatial data
data <- st_read("EDDMapS/Processed_data/points_clean.shp")

# Download New Mexico state boundaries
state <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>% 
  filter(ID %in% c("new mexico")) %>% #, "arizona", "texas", "colorado"
  st_transform(crs = st_crs(data))

#read in the shapefile of spatial data
data <- st_read("EDDMapS/Processed_data/points_clean.shp") %>%
  mutate(year = year(date_ent)) # %>%
  #filter(disp_name == "African rue") 

#make hexgrid
grid <- st_make_grid(state, square = FALSE, cellsize=0.09) #0.009 is about 1km
# plot(grid)
# plot(state, add=TRUE, border="red", col=NA, lwd=2)
  

join <- st_join(st_sf(geometry = grid), data) %>%
  group_by(geometry, disp_name) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(!is.na(disp_name))
  

# Plot hex grid counts
plot(join["count"], reset = FALSE, lwd=0.05)

ggplot(join)+
  geom_sf(aes(fill=count), color=NA) +
  geom_sf(data=state, fill=NA, color="red")+
  scale_fill_viridis_c(option="plasma", direction=1, alpha = 0.5)+
theme_classic() +
  facet_wrap(~disp_name)
  


################################################################################
#### GET SOIL DATA FOR POINTS

# Convert to WGS84 (SSURGO needs lat/long)
pts_wgs <- st_transform(data, 4326)
# 
# library(purrr)
# 
# # Split into groups of e.g. 100 points
# pts_list <- split(pts_wgs, ceiling(seq_len(nrow(pts_wgs)) / 100))
# 
# # Run queries in batches
# results <- map(pts_list, ~ SDA_spatialQuery(.x, what = "mukey"))
# 
# # Combine results
# ssurgo <- do.call(rbind, results)

#too many points - times out
# Query SSURGO data by coordinates
ssurgo <- SDA_spatialQuery(pts_wgs, what = "mukey")
head(ssurgo)

# Get component-level soil texture data
comp <- get_component_data_from_SDA(WHERE = paste0("mukey IN (", paste(ssurgo$mukey, collapse=","), ")"))

head()

WHERE <- paste0("mukey IN (", paste(ssurgo$mukey, collapse = ","), ")")

hz <- SDA_query(paste0("
  SELECT mu.mukey, compname, comppct_r, hzdept_r, hzdepb_r, sandtotal_r, silttotal_r, claytotal_r
  FROM legend AS l
  INNER JOIN mapunit AS mu ON mu.lkey = l.lkey
  INNER JOIN component AS c ON c.mukey = mu.mukey
  INNER JOIN chorizon AS ch ON ch.cokey = c.cokey
  WHERE mu.mukey IN (", paste(ssurgo$mukey, collapse = ","), ")
"))

head(hz)

###############################################GRAB TEXTURE DATA FROM SDA

# empty list to hold results
results <- list()

# loop through each point
for (i in seq_len(nrow(pts_wgs))) {
  pt <- pts_wgs[i, ]   # single point
  oid <- pt$objectid   # unique ID
  
  # try SDA query for mukey
  tryCatch({
    mukey_res <- SDA_spatialQuery(pt, what = "mukey")
    
    if (!is.null(mukey_res) && nrow(mukey_res) > 0) {
      mukey <- mukey_res$mukey[1]   # take first mukey (most detailed)
      
      # query surface horizon textures for that mukey
      tex_query <- sprintf("
        SELECT mu.mukey, ch.hzdept_r, ch.sandtotal_r, ch.silttotal_r, ch.claytotal_r
        FROM legend lg
        JOIN mapunit mu ON lg.lkey = mu.lkey
        JOIN component co ON mu.mukey = co.mukey
        JOIN chorizon ch ON co.cokey = ch.cokey
        WHERE mu.mukey = %s
        AND ch.hzdept_r = 0
      ", mukey)
      
      tex_res <- SDA_query(tex_query)
      
      if (nrow(tex_res) > 0) {
        res <- data.frame(
          objectid = oid,
          mukey    = mukey,
          sand     = tex_res$sandtotal_r[1],
          silt     = tex_res$silttotal_r[1],
          clay     = tex_res$claytotal_r[1]
        )
      } else {
        res <- data.frame(objectid = oid, mukey = mukey,
                          sand = NA, silt = NA, clay = NA)
      }
    } else {
      res <- data.frame(objectid = oid, mukey = NA,
                        sand = NA, silt = NA, clay = NA)
    }
    
    results[[i]] <- res
  }, error = function(e) {
    message("Error at point ", oid, ": ", e$message)
    results[[i]] <- data.frame(objectid = oid, mukey = NA,
                               sand = NA, silt = NA, clay = NA)
  })
  
  # optional: progress update
  if (i %% 100 == 0) message("Processed ", i, " points...")
}

# bind all results into one dataframe
soil_results <- bind_rows(results)

# join back to original points
pts_with_soil <- pts_wgs %>%
  left_join(soil_results, by = "objectid")


# ############################################USE LOCAL gSSURGO POLYGON
# #faster and safer than 13k queries to SDA
# 
# # Load NM gSSURGO mapunit polygons (has mukey field)
# gssurgo_mu <- st_read("path/to/gSSURGO_NM.gdb", layer = "MapunitPoly")
# 
# # Make sure CRS matches
# pts_wgs <- st_transform(pts_wgs, st_crs(gssurgo_mu))
# 
# # Spatial join: assign mukey to each point
# pts_mukey <- st_join(pts_wgs, gssurgo_mu["mukey"])

#############################################TERNARY SOIL PLOT BY SPECIES

for (i in unique(pts_with_soil$disp_name)) {
  
  p <- ggtern(
    data = pts_with_soil %>% filter(disp_name == i),
    aes(x = sand, y = silt, z = clay)) +
    
    geom_point(size = 4, alpha = 0.35, color = "red") +  # set color here
    labs(title = paste0(i, " - soil texture")) +
    
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white", color = NA),    # white plot background
          panel.background = element_rect(fill = "white", color = NA),   # white panel background
          tern.panel.grid.minor = element_line(color = "grey70"),        # minor grid lines color
          tern.panel.grid.major = element_line(color = "grey70"),        # major grid lines color
          tern.axis.ticks = element_line(color = "grey70"),              # axis ticks
          tern.axis.line.T = element_line(color = "grey70"),             # top axis line
          tern.axis.line.L = element_line(color = "grey70"),             # left axis line
          tern.axis.line.R = element_line(color = "grey70"))             # right axis line

  print(p)
  
ggsave()
}
  





#############################################PLOT OBSERVATIONS THROUGH TIME
data_sum <- data_f %>%
  group_by(year, disp_name) %>%
  summarise(records = n())

graphics.off()

ggplot(data_sum) +
  geom_line(aes(x = year, y = records, color=disp_name))+
#  geom_col() +
  theme_minimal() +
  labs(
    title = "Number of Invasive Plant Records Through Time",
    x = "Year",
    y = "Number of Records"
  )

