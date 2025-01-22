#leaflet map of observations and ecoregions
#https://library.virginia.edu/data/articles/data-scientist-as-cartographer-an-introduction-to-making-interactive-maps-in-r-with-leaflet
#leaflet maps: https://leaflet-extras.github.io/leaflet-providers/preview/

library(leaflet)
library(magrittr)
library(sf)
library(terra)
library(geojsonio)
library(htmltools)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)
library(viridis)
library(readxl)
library(tidyverse)
library(leaflet.extras)

##### Prepare data for the leaflet map
#read in shapefile of the SWCDs
SWCDs <- read_sf('data/shapefiles/nmswcd/nmswcd.shp') %>%
  mutate(NAME = ifelse(NAME=="Edgewood", "Tri-County", NAME))
#st_crs(SWCDs)
SWCDs <- st_make_valid(st_transform(SWCDs, crs = '+proj=longlat +datum=WGS84')) #convert shapefile to match leaflet map
#plot(SWCDs)

#Use data gathered from the Access_iNat_data.R script
inat_obs_sf <- read.csv("iNaturalist/data/inat_raw_data_2025-01-22.csv") %>%
  select(longitude, latitude, datetime, common_name, scientific_name, quality_grade) %>% #selects specific vectors [columns of data]
  mutate(date = date(ymd_hms(datetime, tz="America/Denver"))) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) #define coordinate reference system for original data

inat_obs_sf <- st_make_valid(st_transform(inat_obs_sf, crs = '+proj=longlat +datum=WGS84')) #convert shapefile to match leaflet map


#calculate the percentage of each sp.for all observations within a SWCD
ggplot() +
  geom_sf(data=SWCDs)+
  geom_sf(data=inat_obs_sf, aes(color=scientific_name)) +
  theme_classic() +
  theme(legend.position = "none")

#join the SWCD name to the points file

points_SWCD <- st_join(inat_obs_sf, SWCDs, join=st_intersects) %>%
  filter(!is.na(NAME))
  
#calculate the % of each veg type by SWCD
counts <- points_SWCD %>%
  group_by(NAME, scientific_name) %>%
  summarize(count=n())

#calculate the total count of observations and join count data
totals <- points_SWCD %>%
  group_by(NAME) %>%
  summarize(total_count = n()) %>%
  ungroup() %>%
  st_join(counts%>%select(-geometry)) %>%
  mutate(percent = (count / total_count)*100,
         percent = round(percent, digits=1)) %>%
  rename(NAME = NAME.x)

totals_table <- totals %>%
  as_tibble() %>%
  select(NAME, scientific_name, count, percent) %>%
  arrange(desc(percent))

#join count/percentages to shapefile and create popup content
SWCDs_vals <- SWCDs %>%
  left_join(totals_table) %>%
  group_by(NAME) %>%
  summarize(popup_content = paste0("<em>", scientific_name,"</em>",": ", percent, "%", collapse = "<br>")) %>%
  ungroup() %>%
  mutate(popup_content = ifelse(grepl("NA", popup_content),"No Records",popup_content))

#fun colors for points
species_palette <- colorFactor(palette = turbo(n = length(unique(inat_obs_sf$scientific_name))),
                               domain = inat_obs_sf$scientific_name)
##### Create custom formating 
add_custom_layer_control <- function(map) {
  htmlwidgets::onRender(map, '
    function(el, x) {
      var layerControl = document.querySelector(".leaflet-control-layers");
      if (layerControl) {
        // Create custom header for base maps
        var baseMapsHeader = document.createElement("div");
        baseMapsHeader.className = "custom-layer-control-header";
        baseMapsHeader.innerHTML = "<strong>Base Maps</strong>";
        
        // Insert custom headers into the layer control
        layerControl.insertBefore(baseMapsHeader, layerControl.firstChild);
        layerControl.insertBefore(layersHeader, layerControl.firstChild.nextSibling);
        
        // Apply custom CSS styling
        var style = document.createElement("style");
        style.textContent = `
          .custom-layer-control-header {
            #background-color: #ffffff;
            # padding: 10px;
            # border-radius: 5px;
            # border: 1px solid #ccc;
            # margin-bottom: 10px;
            font-size: 14px;
            font-weight: bold;
          }
          .leaflet-control-layers {
            padding: 0;
          }
        `;
        document.head.appendChild(style);
      }
    }
  ')
}
  
#####create a leaflet map
nox_pcnt <- leaflet() %>% #initializes the map widget
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Simple") %>%
  addProviderTiles(providers$CyclOSM, group = "CyclOSM") %>%
  
  #good but not working
  #addProviderTiles(provider = "Stadia.AlidadeSmoothDark", group = "Dark") %>%
  # addProviderTiles(providers$Stadia.StamenTerrain, group = "Terrain") %>%
  # addProviderTiles(providers$Thunderforest.Landscape, group = "Landscape") %>%
  
  
  #options not used
  # addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
  # addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%

  #control layer ordering
  addMapPane("points", zIndex = 420) %>%
  
  addPolygons(data=SWCDs_vals,
              group="SWCDs",
              #fillColor = ~colorFactor("viridis", domain=NULL)(NAME),
              fillColor = ,
              color = "#ff2f93",
              dashArray = "3,3",
              weight = 1,
              fillOpacity = 0,
              popup = ~paste(     "<strong>District:</strong>", NAME, "<br>",
                                  "<br><strong>Record frequency (%)</strong><br>",
                                  popup_content)
                
              ) %>%
  addCircleMarkers(data=inat_obs_sf,
                   group="Observations",
                   radius = 1.55,
                   color = ~species_palette(scientific_name),  # Apply the color palette
                   #label = ~District_CWMA,
                   #clusterOptions=markerClusterOptions(),
                   options = pathOptions(pane = "points"),
                   labelOptions = labelOptions(
                     noHide = FALSE,
                     textOnly = TRUE,
                     style = list("color" = "black", "font-weight" = "bold"),
                     textsize = "10px",
                     direction = "auto"),
                   popup = ~paste("<strong>Species:</strong><br><em>", scientific_name, "</em><br>",
                                  "<strong>Observation Date:</strong><br>", date, "<br>")) %>%
            
  addLayersControl(
    baseGroups = c("Simple","Open Street Map","CyclOSM","ESRI World Imagery"),
    overlayGroups = c("Observations","SWCDs"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  
  addScaleBar(position = "bottomleft") # Position can be "bottomleft", "bottomright", "topleft", or "topright"

  nox_pcnt <- add_custom_layer_control(nox_pcnt)
  
nox_pcnt              
              
saveWidget(nox_pcnt, file = 'iNaturalist/iNat_weed_obs.html')        



