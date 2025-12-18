library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(DT)

# USGS NHD WFS service (hydrography)
nhd_wfs <- "https://hydro.nationalmap.gov/arcgis/services/nhd/MapServer/WFSServer?"

ui <- fluidPage(
  titlePanel("New Mexico Water Features in Drawn Polygon or Uploaded KML"),
  fluidRow(
    column(8,
           leafletOutput("map", height = 600),
           fileInput("kml_file", "Upload KML file", accept = c(".kml"))
    ),
    column(4,
           h4("Water Features in Polygon"),
           DTOutput("table"))
  )
)

server <- function(input, output, session) {
  
  # Base map centered on New Mexico
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -105.9, lat = 34.4, zoom = 6) %>%
      addDrawToolbar(
        targetGroup = 'drawnPoly',
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        rectangleOptions = TRUE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        polylineOptions = FALSE,
        editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)
      ) %>%
      addLayersControl(overlayGroups = c('drawnPoly', 'uploaded'), options = layersControlOptions(collapsed=FALSE))
  })
  
  process_polygon <- function(poly) {
    # Ensure we have sfc_POLYGON
    if (inherits(poly, "sf")) poly <- st_geometry(poly)
    if (length(poly) > 1) poly <- poly[[1]]  # take first geometry if multiple
    
    coords <- st_coordinates(poly)[, 1:2, drop = FALSE]  # X/Y columns
    
    # Ensure polygon is closed
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
      poly <- st_polygon(list(coords)) %>% st_sfc(crs = 4326)
    }
    
    # Get bounding box
    bbox <- st_bbox(poly)
    
    # Pull NHD area features (lakes, reservoirs, wetlands)
    areas <- tryCatch({
      st_read(nhd_wfs,
              layer = "nhd:NHDArea",
              wfs_filter = paste0("BBOX(shape,", bbox[2], ",", bbox[1], ",", bbox[4], ",", bbox[3], ")"))
    }, error = function(e) NULL)
    
    # Pull NHD flowlines (rivers, streams)
    flowlines <- tryCatch({
      st_read(nhd_wfs,
              layer = "nhd:NHDFlowline",
              wfs_filter = paste0("BBOX(shape,", bbox[2], ",", bbox[1], ",", bbox[4], ",", bbox[3], ")"))
    }, error = function(e) NULL)
    
    hits_df <- data.frame(Message = "No water features found in this polygon")
    
    if (!is.null(areas) && nrow(areas) > 0) {
      hits_area <- st_intersects(areas, poly, sparse = FALSE)
      hits_area <- areas[apply(hits_area, 1, any), ] %>% st_drop_geometry()
      hits_df <- hits_area
    }
    
    if (!is.null(flowlines) && nrow(flowlines) > 0) {
      hits_flow <- st_intersects(flowlines, poly, sparse = FALSE)
      hits_flow <- flowlines[apply(hits_flow, 1, any), ] %>% st_drop_geometry()
      if (nrow(hits_flow) > 0) {
        if ("Message" %in% names(hits_df)) {
          hits_df <- hits_flow
        } else {
          hits_df <- bind_rows(hits_df, hits_flow)
        }
      }
    }
    
    hits_df
  }
  
  # React to new polygon being drawn
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    coords <- NULL
    if (feat$geometry$type %in% c("Polygon", "Rectangle")) {
      coords <- feat$geometry$coordinates[[1]][[1]]
    }
    
    if (!is.null(coords)) {
      coords <- do.call(rbind, coords)
      coords <- matrix(as.numeric(coords), ncol = 2)
      poly <- st_polygon(list(coords)) %>% st_sfc(crs = 4326)
      
      hits_df <- process_polygon(poly)
      output$table <- renderDT({datatable(hits_df, options = list(pageLength = 5))})
    }
  })
  
  # React to uploaded KML
  observeEvent(input$kml_file, {
    req(input$kml_file)
    kml_poly <- st_read(input$kml_file$datapath, quiet = TRUE) %>% st_transform(4326)
    
    # Take first feature and get its sfc geometry
    poly_geom <- st_geometry(kml_poly[1, , drop = FALSE])
    
    leafletProxy("map") %>%
      clearGroup("uploaded") %>%
      addPolygons(data = poly_geom, color = "blue", weight = 2, fill = FALSE, group = "uploaded")
    
    hits_df <- process_polygon(poly_geom)
    output$table <- renderDT({datatable(hits_df, options = list(pageLength = 5))})
  })
}

shinyApp(ui, server)
