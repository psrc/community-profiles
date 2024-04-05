create_displacement_risk_map <- function(shape_tract, shape_place, title) {
  # Generate tract shape cut to place of interest and display in leaflet
  
  # Trim Tracts for current place
  shape_place_valid <- st_make_valid(shape_place)
  shp_cut_ids <- st_intersection(shape_tract, shape_place_valid)
  
  # remove non-relevant tracts from spatial intersection output
  if(str_detect(shape_place$geog_name, "County")) {
    cnty_code <- switch(shape_place$geog_name,
                        "King County" = "033",
                        "Kitsap County" = "035",
                        "Pierce County" = "053",
                        "Snohomish County" = "061")
    shp_cut_ids <- shp_cut_ids %>% 
      filter(countyfp10  == cnty_code)
  }
  
  shp_cut <- shape_tract %>% 
    filter(geoid10 %in% shp_cut_ids$geoid10) %>% 
    mutate(risk_level_name = str_to_title(risk_level_name)) %>% 
    mutate(risk_level_name = factor(risk_level_name, levels = c('Lower', 'Moderate', 'Higher')))
  
  dispal <- colorFactor(palette = c("#5ab4ac", "#FFFF8F", "#d8b365"), 
                        levels = c('Lower', 'Moderate', 'Higher'), na.color = "grey")
  
  title <- tags$div(HTML(title))
  
  labels <- paste0("<b>Tract ", shp_cut$geoid10,"</b>", "<br>Displacement Risk Level: ", shp_cut$risk_level_name) %>%
    lapply(htmltools::HTML)
  
  ## Create Map ----
  
  m <- leaflet(data = shp_cut, options = leafletOptions(zoomControl=FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Place Boundary", "Census Tracts"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addPolygons(weight = 1,
                opacity = 1,
                fillColor = ~dispal(risk_level_name),
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  color = "#80fdff",
                  weight = 3.5,
                  dashArray ="",
                  fillOpacity = 0.7
                ),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                group = "Census Tracts") %>%
    addPolygons(data = shape_place,
                fill = FALSE,
                weight = 3,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
                group = "Place Boundary")%>%
    addControl(title, position = "topleft") %>% 
    addLegend("bottomleft",
              pal = dispal,
              values = ~risk_level_name,
              title = "Risk Level")
  
  return(m)
}