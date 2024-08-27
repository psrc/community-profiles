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
                  fillOpacity = 0.7#,
                  # bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                group = "Census Tracts") %>%
    addPolygons(data = city,
                fill = FALSE,
                weight = 3,
                opacity = .75,
                color = "#91268F",
                dashArray = "4",
                group = "Place Boundary")%>% 
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