# Functions for use in app creation
return_estimate <- function(t, p, y, v, val, d) {
  
  r <- t %>%
    filter(geog_name %in% p) %>%
    filter(census_year %in% y) %>%
    filter(variable_name %in% v) %>%
    select(.data[[val]]) %>%
    sum()
  
  r <- round(r,d)
  
  result <- format(r, big.mark = ",")
  
  return(result)
}

find_place_data <- function(p, v) {
  c <- as.numeric(community.point %>% filter(NAME == p) %>% select(.data[[v]]))
  return(c)
}

table_cleanup <- function(w_tbl,curr_cols,upd_cols) {
  w_tbl <-w_tbl[,..curr_cols]
  setnames(w_tbl,upd_cols)  
  return(w_tbl)
}

create_summary_table <- function(t,p,y,cat,v,c,tot,r=NULL,o) {
  # Subset the table and add a share of the total results
  tbl <- t %>% 
    filter(geog_name %in% p) %>%
    filter(census_year %in% y) %>%
    filter(.data[[cat]] %in% v) %>%
    select(variable_description,estimate,margin_of_error)
  
  # Remove any extraneous columns if r is not NULL
  if (!is.null(r)) {
    tbl <- tbl %>% filter(!(variable_description %in% r))
  }
  
  total <- tbl %>% filter(variable_description %in% tot) %>% select(estimate) %>% sum() %>% as.integer()
  
  tbl <- tbl %>% 
    mutate(Share = estimate/total) %>% 
    filter(!(variable_description %in% tot)) %>%
    mutate(variable_description = factor(variable_description, levels=o))
  
  r.tbl <- t %>%
    filter(geog_name %in% c("King County","Kitsap County", "Pierce County", "Snohomish County")) %>%
    filter(census_year %in% y) %>% 
    filter(.data[[cat]] %in% v) %>%
    select(variable_description,estimate,margin_of_error)

  if (!is.null(r)) {
    r.tbl <- r.tbl %>% filter(!(variable_description %in% r))
  }
  
  # Combine County Results into a Regional Total
  regional <- r.tbl %>% 
    group_by(variable_description) %>% 
    summarise(estimate = sum(estimate))
  
  total <- regional %>% filter(variable_description == tot) %>% select(estimate) %>% sum() %>% as.integer()
  
  regional <- regional %>% 
    mutate(Share = estimate/total) %>% 
    filter(!(variable_description %in% tot)) %>%
    mutate(variable_description = factor(variable_description, levels=o)) %>%
    rename(Region=Share) %>%
    select(-estimate)

  # Merge the Place and Region tables on variable description
  tbl <- left_join(tbl,regional,by="variable_description")
  
  return(tbl)
}

create_summary_chart <- function(d, p, y, v, d.var, d.cols, d.tot, d.rem, d.ord, d.title, d.clr) {
  
  t <- create_summary_table(d, p, y, v, d.var, d.cols, d.tot, d.rem, d.ord)
  t.max <- t %>% select(Share) %>% pull() %>% max() %>% as.numeric()
  t.max <- round(t.max * 1.1,1)
  
  g <- ggplotly(
    ggplot(data=t, 
         aes(x=`variable_description`, 
             y=`Share`,
             text= paste0("<b>", d.title, ": ","</b>",prettyNum(round(`Share`*100, 1), big.mark = ","),"%")
         )) +
    geom_bar(stat="identity", color = d.clr, fill = d.clr) +
    scale_y_continuous(labels = scales::percent, limits = c(0, t.max))+
    coord_flip() +
    theme(legend.position = "none",
          axis.title.y=element_blank(), 
          axis.title.x=element_blank(),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12,face="bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank())
  ,tooltip = c("text"))

  return(g)
}

create_tract_map_pick_variable <- function(t, v, y, d.clr, p, e, v.type, w.title, w.pre, w.suff) {
  
  # Trim full Tract table to Variable and Year of interest
  current_tbl <- t %>% 
    filter(census_year %in% y) %>%
    filter(.data[[v.type]] %in% v) %>%
    filter(place_type %in% "tr") %>%
    select(geoid, .data[[e]]) %>%
    rename(value=.data[[e]]) %>%
    mutate(value = case_when(
      value<=0 ~0,
      value>0 ~value)) %>%
    mutate(geoid = as.character(geoid))

  # Trim Tracts for current place
  city <- community.shape %>% filter(NAME %in% p)
  interim <- st_intersection(tract.shape, city)
  tract_ids <- interim %>% st_drop_geometry() %>% select(GEOID10) %>% distinct() %>% pull()
  
  tracts.trimmed <- tract.shape %>% filter(GEOID10%in% tract_ids)
  current_value  <- left_join(tracts.trimmed, current_tbl, by=c("GEOID10"="geoid"))
  
  # Determine Bins
  rng <- range(current_value$value)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to)*round_to
  breaks <- (max_bin*c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1))
  bins <- c(0, breaks)
  
  pal <- colorBin(d.clr, domain = current_value$value, bins = bins)
  
  labels <- paste0("<b>",paste0(w.title,": "), "</b>", w.pre, prettyNum(round(current_value$value, 1), big.mark = ","),w.suff) %>% lapply(htmltools::HTML)
  
  # Create Map
  working_map <- leaflet(data = current_value, options = leafletOptions(zoomControl=FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Census Tracts","City Boundary"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
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
                group = "Census Tracts")%>%
    addLegend("bottomright", pal=pal, values = current_value$value,
              title = paste0(w.title),
              labFormat = labelFormat(prefix = w.pre, suffix = w.suff),
              opacity = 1) %>%
    setView(lng=find_place_data(p,"INTPTLON"), lat=find_place_data(p,"INTPTLAT"), zoom=find_place_data(p,"ZOOM"))
  
  return(working_map)
  
}
