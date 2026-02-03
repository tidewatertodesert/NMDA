library(terra)
library(sf)
library(tigris)
library(exactextractr)
library(dplyr)
library(ggplot2)

ndvi_anom <- rast("C:/Users/dburruss/Documents/GIS/Drought_Monitor/GIMMS_data/10.2025/GMOD09Q1.A2025281.08d.latlon.global.061.NDVI_anom_S2001-2024.tif")  # specify full filename

#Check info
ndvi_anom
# plot(ndvi_anom)

#Get NM state and county boundaries, transform to raster CRS
nm_state <- states(cb = TRUE) %>%
  filter(STUSPS == "NM") %>%
  st_transform(crs(ndvi_anom))

nm_counties <- counties(state = "NM", cb = TRUE) %>%
  st_transform(crs(ndvi_anom))

#Crop and mask NDVI to NM
ndvi_nm <- crop(ndvi_anom, nm_state)
ndvi_nm <- mask(ndvi_nm, nm_state)

plot(ndvi_nm)

hist(ndvi_nm)

#center on 125 for plotting
ndvi_anom <- (ndvi_nm - 125)# * 0.008 * 125

df_map <- as.data.frame(ndvi_anom, xy = TRUE, na.rm = TRUE)
names(df_map)[3] <- "value"

breaks <- c(-250,-80,-60,-40,-30,-20,-10,-5,5,10,20,30,40,60,80,250)
labels <- paste0(head(breaks, -1), " to ", tail(breaks, -1))

df_map <- df_map %>%
  mutate(bin = cut(value, breaks = breaks, labels = labels, include.lowest = TRUE))

colors_manual <- c( "#ff0000",
                    "#64321e",
                    "#c8643c",
                    "#ffaf5a",
                    "#ffd094",
                    "#fae6c8",
                    "#fff564",
                    "#fffff5",
                    "#d4ff8c",
                    "#9cf593",
                    "#7ad214",
                    "#28af69",
                    "#0aa00a",
                    "#006400",
                    "#963282" )

plot(df_map$value)

ggplot() +
  geom_raster(data = df_map, aes(x = x, y = y, fill = bin)) +
  geom_sf(data = nm_counties, fill = NA, color = "gray25", linewidth = 0.2) +
  scale_fill_manual(
    values = colors_manual,
    name = "NDVI anomaly (%)",
    drop = FALSE
  ) +
  coord_sf() +
  theme_classic() +
  theme(
    legend.key.height = unit(0.6, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    panel.grid = element_blank()
  ) +
  labs(
    title = "NDVI Percent Anomaly – New Mexico",
    subtitle = "GIMMS NDVI_pasg (percent of 2001–2024 mean)",
    x = NULL, y = NULL
  )
