# Functions for use in app creation

create_project_map <- function(p) {
  
  city <- community.shape %>% filter(NAME %in% p)
  interim <- st_intersection(rtp.shape, city)
  proj_ids <- interim %>% st_drop_geometry() %>% select(mtpid) %>% distinct() %>% pull()

  if (is.null(interim) == TRUE) {
    
    working_map <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c("Approved Projects","Candidate Projects","Unprogrammed Projects","City Boundary"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addPolygons(data = city,
                  fillColor = "76787A",
                  weight = 1,
                  opacity = 1.0,
                  color = "#444444",
                  fillOpacity = 0.10,
                  group = "City Boundary")
    
  } else {
    
    rtp.trimmed <- rtp.shape %>% filter(mtpid %in% proj_ids)
    
    candidate <- rtp.trimmed %>% filter(MTPStatus %in% "Candidate")
    approved <- rtp.trimmed %>% filter(MTPStatus %in% "Approved")
    unprogrammed <- rtp.trimmed %>% filter(MTPStatus %in% "Unprogrammed")
    
    approved_labels <- paste0("<b>","Project Sponsor: ", "</b>",approved$Sponsor,
                              "<b> <br>",paste0("Project Title: "), "</b>", approved$Title,
                              "<b> <br>",paste0("Project Cost: $"), "</b>", prettyNum(round(approved$TotalCost, 0), big.mark = ","),
                              "<b> <br>",paste0("Project Status: "), "</b>", approved$MTPStatus,
                              "<b> <br>",paste0("Project Completion: "), "</b>", approved$CompletionYear) %>% lapply(htmltools::HTML)
    
    candidate_labels <- paste0("<b>","Project Sponsor: ", "</b>",candidate$Sponsor,
                               "<b> <br>",paste0("Project Title: "), "</b>", candidate$Title,
                               "<b> <br>",paste0("Project Cost: $"), "</b>", prettyNum(round(candidate$TotalCost, 0), big.mark = ","),
                               "<b> <br>",paste0("Project Status: "), "</b>", candidate$MTPStatus,
                               "<b> <br>",paste0("Project Completion: "), "</b>", candidate$CompletionYear) %>% lapply(htmltools::HTML)    
    
    unprogrammed_labels <- paste0("<b>","Project Sponsor: ", "</b>",unprogrammed$Sponsor,
                                  "<b> <br>",paste0("Project Title: "), "</b>", unprogrammed$Title,
                                  "<b> <br>",paste0("Project Cost: $"), "</b>", prettyNum(round(unprogrammed$TotalCost, 0), big.mark = ","),
                                  "<b> <br>",paste0("Project Status: "), "</b>", unprogrammed$MTPStatus,
                                  "<b> <br>",paste0("Project Completion: "), "</b>", unprogrammed$CompletionYear) %>% lapply(htmltools::HTML)
    # Create Map
    working_map <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c("Approved Projects","Candidate Projects","Unprogrammed Projects","City Boundary"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addPolygons(data = city,
                  fillColor = "76787A",
                  weight = 4,
                  opacity = 1.0,
                  color = "#91268F",
                  dashArray = "4",
                  fillOpacity = 0.0,
                  group = "City Boundary")%>% 
      addPolylines(data = approved,
                   color = "#91268F",
                   weight = 4,
                   label = approved_labels,
                   fillColor = "#91268F",
                   group = "Approved Projects") %>%
      addPolylines(data = candidate,
                   color = "#8CC63E",
                   weight = 4,
                   label = candidate_labels,
                   fillColor = "#8CC63E",
                   group = "Candidate Projects") %>%
      addPolylines(data = unprogrammed,
                   color = "#00A7A0",
                   weight = 4,
                   dashArray = "4",
                   label = unprogrammed_labels,
                   fillColor = "#00A7A0",
                   group = "Unprogrammed Projects")
  }
  
  return(working_map)
  
}
