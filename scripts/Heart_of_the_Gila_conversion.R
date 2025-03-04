library(sf)
library(tidyverse)

sf_object <- st_read("C:/Users/dburruss/Documents/GitHub/NMDA/Gen_assist/Heart of the Gila/FY2024NMWildBullThistleTreatment.gdb/FY2024NMWildBullThistleTreatment.gdb")

names(sf_object)

#save shapefile as single part
st_write(sf_object, "C:/Users/dburruss/Documents/GitHub/NMDA/Gen_assist/Heart of the Gila/KML_out/Combined.kml", driver = "KML")


#Write out individual files for each feature
unique_names <- unique(sf_object$NAME)


# Loop through unique names and write each as a separate KML file
for (i in seq_along(unique_names)) {
  subset_sf <- sf_object %>% filter(NAME == unique_names[i])
  
  # Define output file name
  output_file <- paste0("C:/Users/dburruss/Documents/GitHub/NMDA/Gen_assist/Heart of the Gila/KML_out/Feature_OID_", i, ".kml")
  
  # Write to KML
  st_write(subset_sf, output_file, driver = "KML")
}
