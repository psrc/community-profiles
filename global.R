library(DT)

library(shiny)
library(shinyjs)
library(shinyBS)

# library(ggplot2)
library(scales)
library(plotly)
library(foreign)

library(leaflet)
library(sf)

library(tidyverse)
library(data.table)

library(here)
library(shinycssloaders)
library(psrcplot)
library(echarts4r)

# run all files in the modules sub-directory
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

plan.clrs <- list("Approved" = "#AD5CAB",
                  "Conditionally Approved" = "#C388C2",
                  "ROW Conditionally Approved" = "#E3C9E3",
                  "Candidate" = "#F4835E",
                  "Financially Constrained" = "#F7A489",
                  "Unprogrammed" = "#FBD6C9",
                  "2021-2024 TIP" = "#A9D46E")

# Jurisdiction Data ----
jurisdictions <- as_tibble(fread('data//jurisdictions.csv'))
census_data <- as_tibble(fread('data//census_data_by_place.csv')) %>%
  mutate(Label = gsub("Education & Health Services", "Health & Edu", Label)) %>%
  mutate(Label = gsub("Entertainment, Accommodation & Food Services", "Food & Entertainment", Label)) %>%
  mutate(acs_year = paste0(census_year-4,"-", census_year," ACS Data"))

# Shapefiles ----
wgs84 <- 4326

city.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/City_Boundaries/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  select(city_name, cnty_name) %>% 
  mutate(cnty_name=paste0(cnty_name, " County")) %>%
  mutate(city_name = gsub("Sea Tac","SeaTac",city_name)) %>%
  mutate(city_name = gsub("Beaux Arts","Beaux Arts Village",city_name)) %>%
  rename(geog_name=city_name, county=cnty_name)

rgeo.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Geographies/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  filter(juris=="Silverdale UGA") %>%
  mutate(juris=str_replace(juris, "Silverdale UGA", "Silverdale")) %>%
  select(juris, cnty_name) %>% 
  mutate(cnty_name=paste0(cnty_name, " County")) %>%
  rename(geog_name=juris, county=cnty_name)

county.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/County_Boundaries/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  filter(psrc == 1) %>%
  select(county_nm) %>% rename(geog_name=county_nm) %>%
  mutate(geog_name = paste0(geog_name, " County"), county=geog_name)

community.shape <- rbind(city.shape, county.shape, rgeo.shape)
community.shape <- left_join(community.shape, jurisdictions, by=c("geog_name"="juris_name"))
rm(city.shape, county.shape, rgeo.shape)

community.point <- community.shape %>% st_drop_geometry()

tract.2010 <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Census_Tracts_2010/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>% 
  select(geoid10, county_name) %>%
  mutate(county = paste0(county_name, " County")) %>%
  select(-county_name) %>%
  rename(geoid=geoid10) %>%
  mutate(census_year = 2010)

tract.2020 <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Census_Tracts_2020/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>% 
  select(geoid20, county_name) %>%
  mutate(county = paste0(county_name, " County")) %>%
  select(-county_name) %>%
  rename(geoid=geoid20) %>%
  mutate(census_year = 2020)

tract.shape <- rbind(tract.2010, tract.2020)
rm(tract.2010, tract.2020)

rtp.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Capacity_Projects/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  mutate(Total_Cost = as.numeric(Total_Cost)) %>%
  mutate(Completion = as.character(Completion)) %>%
  select(MTP_ID, Sponsor, Agency_Typ, Project_Ti, Type, Completion, Status, Total_Cost) %>%
  rename(`ID`=MTP_ID, `Type`=Agency_Typ,`Title`=Project_Ti, `Improvement`=Type, `Cost`=Total_Cost)

tip.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/TIP_21_24/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  mutate(TotCost = as.numeric(TotCost)) %>%
  mutate(EstCompletionYear = as.character(EstCompletionYear)) %>%
  mutate(Status="2021-2024 TIP") %>%
  select(ProjNo,PlaceShortName,ProjectTitle,ImproveType,EstCompletionYear,Status,TotCost) %>%
  rename(`ID`=ProjNo, `Sponsor`=PlaceShortName, `Title`=ProjectTitle) %>%
  rename(`Improvement`=ImproveType, `Completion`= EstCompletionYear, `Status`=Status, `Cost`=TotCost) %>%
  mutate(Type = case_when(
    Sponsor %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County") ~ "Counties",
    Sponsor %in% c("Kitsap Transit", "Pierce Transit", "Everett Transit", "King County Metro", "Community Transit") ~ "Local Transit",
    Sponsor %in% c("Sound Transit") ~ "Regional Transit",
    Sponsor %in% c("Tulalip Tribes", "Port of Seattle", "Muckleshoot Indian Tribe", "Port of Everett") ~ "Ports/Tribes",
    Sponsor %in% c("WSDOT") ~ "State",
    Sponsor %in% c("Washington State Ferries") ~ "WSF")) %>%
  mutate(Type = replace_na(Type, "Cities")) %>%
  select(ID, Sponsor, Type, Title, Improvement, Completion, Status, Cost)

projects.shape <- rbind(tip.shape, rtp.shape)

numeric_variables <- c("Estimate","MoE")
percent_variables <- c("Share","Region")
final.nms <- c("ID","Sponsor","Type","Title","Improvement","Completion","Status","Cost")
currency.rtp <- c("Cost")
rtp.status <- c("Approved","Conditionally Approved","ROW Conditionally Approved","Candidate", "Financially Constrained", "Unprogrammed")

# Functions ----
return_estimate <- function(t, p, y, v, val, d) {
  
  r <- t %>%
    filter(geog_name %in% p) %>%
    filter(acs_year %in% y) %>%
    filter(Label %in% v) %>%
    select(.data[[val]]) %>%
    sum()
  
  if (val == "share") {
    r = round(r*100,d)
  } else {
    r <- round(r,d)
  }
  
  result <- format(r, big.mark = ",")
  
  return(result)
}

find_rgeo_data <- function(p, v) {
  c <- jurisdictions %>% filter(juris_name == p) %>% select(.data[[v]]) %>% pull()
  return(c)
}

create_summary_echart <- function(d, p, y, v, val, f=1, dec=0, s="", d.title, d.clr) {

  js <- paste("function(params, ticket, callback) {
                                       var fmt = new Intl.NumberFormat('en', {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
                                       var idx = 0;\n
                                       if (params.name == params.value[0]) {\n
                                       idx = 1;\n        }\n
                                       return(params.marker + ' ' +\n'",
                                               d.title ," in ' + params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]))
                                              )
                                       }")
  
  t <- d %>% 
    filter(geog_name %in% c(p,"Region") & acs_year == y & Category == v) %>%
    select(Label, .data[[val]], geog_name) %>%
    filter(!(str_detect(Label, "Total"))) %>%
    mutate(Label = str_wrap(Label, 20))
  
  lv <- t %>% filter(geog_name == p) %>% select(Label) %>% pull()
  
  t <- t %>% 
    mutate(Label = factor(Label, levels = lv)) %>%
    mutate(geog_name = factor(geog_name, levels = c(p, "Region")))
  
  t.max <- t %>% select(.data[[val]]) %>% pull() %>% max() %>% as.numeric()
  t.max <- t.max * 1.25
  
  w.pal <- c(d.clr,"#999999")

  e <- t %>% 
    group_by(geog_name) %>% 
    e_charts_(x = "Label", stack = NULL) |>
    e_bar_(val) |>
    # e_y_axis(max = ymax) |>
    # e_y_axis(splitNumber = 3, max = ymax) |>
    e_x_axis(axisLabel = list(interval = 0L),
             axisTick = list(alignWithLabel = TRUE)) |>
    e_flip_coords() |>
    e_grid(left = '21%', top = '10%') |>
    e_color(w.pal) |>
    e_tooltip(formatter =  e_tooltip_item_formatter("percent", digits = 1)) |>
    e_tooltip(formatter =  htmlwidgets::JS(js)) |>
    e_x_axis(formatter = e_axis_formatter("percent", digits = 0)) |>
    e_legend(bottom=0) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  return(e)
}

create_summary_chart <- function(d, p, y, v, val, f=1, dec=0, s="", d.title, d.clr) {
  
  t <- d %>% 
    filter(geog_name %in% c(p,"Region") & acs_year == y & Category == v) %>%
    select(Label, .data[[val]], geog_name) %>%
    filter(!(str_detect(Label, "Total"))) %>%
    mutate(Label = str_wrap(Label, 20))
  
  lv <- t %>% filter(geog_name == p) %>% select(Label) %>% pull()
  
  t <- t %>% 
    mutate(Label = factor(Label, levels = lv)) %>%
    mutate(geog_name = factor(geog_name, levels = c(p, "Region")))
  
  t.max <- t %>% select(.data[[val]]) %>% pull() %>% max() %>% as.numeric()
  t.max <- t.max * 1.25
  
  w.pal <- c(d.clr,"#999999")

  g <- ggplotly(
    ggplot(data=t,
           aes(x=`Label`,
               y=get(eval(val)),
               fill=geog_name,
               text= paste0("<b>", d.title, " in ",geog_name, ": ","</b>",prettyNum(round(get(eval(val))*f, dec), big.mark = ","),s)
           )) +
      geom_bar(stat="identity", position = "dodge") +
      scale_fill_manual(values = w.pal) +
      scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, t.max))+
      coord_flip() +
      theme(legend.position = "bottom",
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank())
    ,tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1, title = ""))

  return(g)
}


create_tract_map <- function(t, y, p, v, val, d.clr, d.title, pre="", s="", dec=0, f=1) {

  if (y == "2016-2020 ACS Data") {
    tracts <- tract.shape %>% filter(census_year==2020)
  } else {
    tracts <- tract.shape %>% filter(census_year==2010)
  }
  
  title <- tags$div(HTML(d.title))  
  
  # Trim full Tract table to Variable and Year of interest
  current_tbl <- t %>% 
    filter(acs_year == y & place_type == "tr" & Label == v) %>%
    select(geoid, .data[[val]]) %>%
    rename(value=.data[[val]]) %>%
    mutate(geoid = as.character(geoid), value = value*f)
  
  if (p %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County")) {
    
    tracts.trimmed <- tracts %>% filter(county == p)
    city <- community.shape %>% filter(geog_name %in% p)
    
  } else {
  
    # Trim Tracts for current place
    city <- community.shape %>% filter(geog_name %in% p)
    interim <- st_intersection(tracts, city)
    tract_ids <- interim %>% st_drop_geometry() %>% select(geoid) %>% distinct() %>% pull()
    tracts.trimmed <- tracts %>% filter(geoid%in% tract_ids)
  }
  
  current_value  <- left_join(tracts.trimmed, current_tbl, by=c("geoid")) %>% drop_na()
  
  # Determine Bins
  rng <- range(current_value$value)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to)*round_to
  breaks <- (max_bin*c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1))
  bins <- c(0, breaks)
  
  pal <- colorBin(d.clr, domain = current_value$value, bins = bins)
  
  labels <- paste0("<b>",paste0(d.title,": "), "</b>", pre, prettyNum(round(current_value$value, dec), big.mark = ","),s) %>% lapply(htmltools::HTML)
  
  # Create Map
  working_map <- leaflet(data = current_value, options = leafletOptions(zoomControl=FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Census Tracts","Place Boundary"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addPolygons(data = city,
                fillColor = "76787A",
                weight = 4,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
                fillOpacity = 0.0,
                group = "City Boundary")%>% 
    addPolygons(fillColor = pal(current_value$value),
                weight = 1.0,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight =5,
                  color = "76787A",
                  dashArray ="",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                group = "Census Tracts") %>%
    addControl(title, position = "topleft")
  
  return(working_map)
  
}

create_summary_table <- function(t,p,y,v) {
  
  # Subset the table and add a share of the total results
  tbl <- t %>% 
    filter(geog_name %in% c(p) & acs_year == y & Category == v) %>%
    select(Label,estimate,margin_of_error,share) %>%
    rename(Variable=Label, Estimate=estimate, MoE=margin_of_error, Share=share)
  
  r <- t %>% 
    filter(geog_name %in% c("Region") & acs_year == y & Category == v) %>%
    select(Label,share) %>%
    rename(Variable=Label, Region=share)

  tbl <- left_join(tbl,r,by="Variable")
  
  return(tbl)
}

create_tip_map <- function(p, i=tip.shape, plan.yr, d.title, d.clr=plan.clrs) {
  
  # First determine the city and trim city shapefile and project coverage to the city
  city <- community.shape %>% filter(geog_name %in% p)
  interim <- st_intersection(i, city) %>% filter(Status %in% plan.yr)
  proj_ids <- interim %>% st_drop_geometry() %>% select(ID) %>% distinct() %>% pull()
  
  title <- tags$div(HTML(d.title)) 
  
  if (is.null(interim) == TRUE) {
    
    working_map <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c(plan.yr,"City Boundary"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addPolygons(data = city,
                  fillColor = "76787A",
                  weight = 1,
                  opacity = 1.0,
                  color = "#444444",
                  fillOpacity = 0.10,
                  group = "City Boundary") %>%
      addControl(title, position = "bottomleft")
    
  } else {
    
    trimmed <- i %>% filter(ID %in% proj_ids & Status %in% plan.yr) %>% mutate(Status=factor(Status, levels=plan.yr))
    
    labels <- paste0("<b>","Project Sponsor: ", "</b>",trimmed$Sponsor,
                     "<b> <br>",paste0("Project Title: "), "</b>", trimmed$Title,
                     "<b> <br>",paste0("Project Cost: $"), "</b>", prettyNum(round(trimmed$Cost, 0), big.mark = ","),
                     "<b> <br>",paste0("Type of Improvement: "), "</b>", trimmed$Improvement,
                     "<b> <br>",paste0("Project Completion: "), "</b>", trimmed$Completion) %>% lapply(htmltools::HTML)
    # Create Map
    working_map <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c(plan.yr,"City Boundary"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addPolygons(data = city,
                  fillColor = "76787A",
                  weight = 4,
                  opacity = 1.0,
                  color = "#91268F",
                  dashArray = "4",
                  fillOpacity = 0.0,
                  group = "City Boundary")%>% 
      addControl(title, position = "bottomleft")
    
    for(group in levels(trimmed$Status)){
      working_map <- addPolylines(working_map, data=trimmed[trimmed$Status==group,], group = group, weight = 4, label = labels, color=d.clr[[group]])
    }
    
  }
  
  return(working_map)
  
}

create_rtp_map <- function(p, i=rtp.shape, plan.yr, d.title, d.clr=plan.clrs) {
  
  # First determine the city and trim city shapefile and project coverage to the city
  city <- community.shape %>% filter(geog_name %in% p)
  interim <- st_intersection(i, city) %>% filter(Status %in% plan.yr)
  proj_ids <- interim %>% st_drop_geometry() %>% select(ID) %>% distinct() %>% pull()
  
  title <- tags$div(HTML(d.title)) 
  
  if (is.null(interim) == TRUE) {
    
    working_map <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c(plan.yr,"City Boundary"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addPolygons(data = city,
                  fillColor = "76787A",
                  weight = 1,
                  opacity = 1.0,
                  color = "#444444",
                  fillOpacity = 0.10,
                  group = "City Boundary") %>%
      addControl(title, position = "bottomleft")
    
  } else {
    
    trimmed <- i %>% filter(ID %in% proj_ids & Status %in% plan.yr) %>% mutate(Status=factor(Status, levels=plan.yr))

    labels <- paste0("<b>","Project Sponsor: ", "</b>",trimmed$Sponsor,
                     "<b> <br>",paste0("Project Title: "), "</b>", trimmed$Title,
                     "<b> <br>",paste0("Project Cost: $"), "</b>", prettyNum(round(trimmed$Cost, 0), big.mark = ","),
                     "<b> <br>",paste0("Type of Improvement: "), "</b>", trimmed$Improvement,
                     "<b> <br>",paste0("Project Completion: "), "</b>", trimmed$Completion) %>% lapply(htmltools::HTML)
    # Create Map
    working_map <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c(plan.yr,"City Boundary"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addPolygons(data = city,
                  fillColor = "76787A",
                  weight = 4,
                  opacity = 1.0,
                  color = "#91268F",
                  dashArray = "4",
                  fillOpacity = 0.0,
                  group = "City Boundary")%>% 
      addControl(title, position = "bottomleft")
    
    for(group in levels(trimmed$Status)){
      working_map <- addPolylines(working_map, data=trimmed[trimmed$Status==group,], group = group, weight = 4, label = labels, color=d.clr[[group]])
    }
    
  }
  
  return(working_map)
  
}

create_project_table <- function(p, i, f=final.nms, plan.yr) {
  
  city <- community.shape %>% filter(geog_name %in% p)
  trimmed <- st_intersection(i, city) %>% filter(Status%in%plan.yr)
  
  if (is.null(trimmed) == TRUE) {
    
    tbl <- setNames(data.table(matrix(nrow = 0, ncol = 8)), f)
    
  } else {
    
    tbl <- setDT(trimmed %>% st_drop_geometry())
    tbl <- tbl[,..f]
    tbl <- tbl[!duplicated(tbl), ]
  }
  
  return(tbl)
  
}

# Dropdown List Creations -------------------------------------------------
latest.census.yr <- census_data %>% select(census_year) %>% distinct() %>% pull() %>% max()
non.overlap.census.yr <- latest.census.yr - 5

data_years <- census_data %>%
  filter(census_year %in% c(non.overlap.census.yr, latest.census.yr)) %>%
  arrange(desc(census_year)) %>%
  select(acs_year) %>%
  distinct() %>%
  pull()

data_places <- census_data %>%
  filter(place_type %in% c("pl")) %>%
  select(geog_name) %>%
  filter(geog_name != "Region") %>%
  distinct() %>%
  pull()

data_counties <- census_data %>%
  filter(place_type %in% c("co")) %>%
  select(geog_name) %>%
  filter(geog_name != "Region") %>%
  distinct() %>%
  pull()
