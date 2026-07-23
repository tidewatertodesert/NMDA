library(terra)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)
library(stringr)

#data downloaded from https://gimms.gsfc.nasa.gov/MODIS/
#view data at https://glam1.gsfc.nasa.gov/
#--------------------------------------------------------------------
# Paths
#UPDATE FOLDER/FILE HERE
#--------------------------------------------------------------------
input_folder <- "C:/Users/dburruss/Documents/GIS/Drought_Monitor/GIMMS_data/2026.05"
output_folder <- file.path(input_folder, "plots")

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

#--------------------------------------------------------------------
# Get list of TIFF files that contain "pasg" or "anom"
#--------------------------------------------------------------------
tif_files <- list.files(input_folder, pattern = "(pasg|anom).*\\.tif$", full.names = TRUE)

#--------------------------------------------------------------------
# Load NM boundaries once
#--------------------------------------------------------------------
nm_state <- states(cb = TRUE) %>%
  filter(STUSPS == "NM")

nm_counties <- counties(state = "NM", cb = TRUE)

#--------------------------------------------------------------------
# Loop through each NDVI anomaly file
#--------------------------------------------------------------------
for (f in tif_files) {
  
  message("Processing: ", basename(f))
  
  # Determine file type (pasg or anom)
  type <- ifelse(str_detect(f, "pasg"), "pasg", "anom")
  
  # Extract composite info (e.g., 08d, 32d, 88d)
  composite <- str_extract(basename(f), "\\d{2}d")
  
  # Read raster
  ndvi_rast <- rast(f)
  
  # Transform boundaries to raster CRS
  nm_state_t <- st_transform(nm_state, crs(ndvi_rast))
  nm_counties_t <- st_transform(nm_counties, crs(ndvi_rast))
  
  # Crop and mask NDVI to NM
  ndvi_nm <- crop(ndvi_rast, nm_state_t)
  ndvi_nm <- mask(ndvi_nm, nm_state_t)
  
  # Convert raster to data frame for plotting
  df_map <- as.data.frame(ndvi_nm, xy = TRUE, na.rm = TRUE)
  names(df_map)[3] <- "value"
  
  #----------------------------------------------------------------
  # Define breaks and colors based on type
  #----------------------------------------------------------------
  if (type == "pasg") {
    breaks <- c(0, 75, 90, 110, 125, 250)
    labels <- paste0(head(breaks, -1), " to ", tail(breaks, -1))
    
    colors_manual <- c("#ff0000", "#ffff00", "#edf5dc", "#52ff00", "#217100")
    
  } else if (type == "anom") {
    breaks <- c(-125,-80,-60,-40,-30,-20,-10,-5,5,10,20,30,40,60,80,125)
    labels <- paste0(head(breaks, -1), " to ", tail(breaks, -1))
    
    colors_manual <- c(
      "#ff0000", "#64321e", "#c8643c", "#ffaf5a", "#ffd094",
      "#fae6c8", "#fff564", "#fffff5", "#d4ff8c", "#9cf593",
      "#7ad214", "#28af69", "#0aa00a", "#006400", "#963282"
    )
  }
  
  # Bin values
  df_map <- df_map %>%
    mutate(bin = cut(value, breaks = breaks, labels = labels, include.lowest = TRUE))
  
  #----------------------------------------------------------------
  # Plot
  #----------------------------------------------------------------
  p <- ggplot() +
    geom_raster(data = df_map, aes(x = x, y = y, fill = bin)) +
    geom_sf(data = nm_counties_t, fill = NA, color = "gray25", linewidth = 0.2) +
    scale_fill_manual(values = colors_manual, name = "NDVI anomaly", drop = FALSE) +
    coord_sf() +
    theme_classic() +
    theme(
      legend.key.height = unit(0.6, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      panel.grid = element_blank()
    ) +
    labs(
      title = paste0("NDVI ", ifelse(type == "pasg", "Percent Anomaly", "Anomaly"), " – ", composite, " Composite"),
      subtitle = "GIMMS NDVI (2001–2024 reference)",
      x = NULL, y = NULL
    )
  
 
  #----------------------------------------------------------------
  # Save plot
  #----------------------------------------------------------------
  out_file <- file.path(output_folder, paste0("NDVI_", type, "_", composite, ".png"))
  ggsave(out_file, plot = p, width = 7, height = 5, dpi = 300)
}

message("✅ All plots saved to: ", output_folder)

