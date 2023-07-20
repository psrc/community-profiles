library(tidyverse)
library(data.table)
source('modules/function-query-sqlite-chas.R')

create_tenure_tract_table <- function() {
  # Generate tract level table for RDI Tenure metric. To be used in tract map.
  
  chas_tables <- 'T9'
  dfs <- gather_tables(juris = 'tract', chas_tables)
  
  re <- c("People of Color",
          "Hispanic or Latino (of any race)",
          "White",
          'All')
  
  # Table 9 Owner-Occupied 
  oo_head <- c(18,13,8,28,33,23,3,2)
  oo <- dfs$T9[sort %in% oo_head, ]
  
  # Table 9 Renter-Occupied
  ro_head <- c(54,49,44,64,69,59,39,38)
  ro <- dfs$T9[sort %in% ro_head, ]
  
  
  dfs <- list(o = oo, r = ro)
  for(d in dfs) {
    d[, `:=`(grouping = fcase(grepl("^American Indian ", race_ethnicity), "People of Color",
                              grepl("^Asian ", race_ethnicity), "People of Color",
                              grepl("^Black ", race_ethnicity), "People of Color",
                              grepl("^Hispanic, any race", race_ethnicity), "Hispanic or Latino (of any race)",
                              grepl("^other ", race_ethnicity), "People of Color",
                              grepl("^Pacific ", race_ethnicity), "People of Color",
                              grepl("^White ", race_ethnicity), "White",
                              grepl("^All", race_ethnicity), "All"))]
  }

  # select common columns
  cols <- c('tract_geoid', 'chas_year', 'estimate', 'grouping', 'tenure')
  dfs <- map(dfs, ~.x[, ..cols])
  df <- rbindlist(dfs)
  
  # summarise
  df <- df[, .(estimate = sum(estimate)), by = eval(cols[which(cols != 'estimate')])]
  
  df[, tenure := fcase(grepl('^Renter', tenure), 'renter_occupied',
                       grepl('^Owner', tenure), 'owner_occupied')]
  # pivot wider
  df <- dcast.data.table(df, chas_year + tract_geoid + grouping ~ tenure, value.var = 'estimate')
  
  # Calculate Table ----
  df[, all_units := owner_occupied + renter_occupied
  ][, `:=` (renter_share = renter_occupied/all_units,
            owner_share = owner_occupied/all_units)]
  
  # pivot longer
  df <- melt.data.table(df, id.vars = c('chas_year', 'tract_geoid', 'grouping'), measure.vars = str_subset(colnames(df), '.*share$'), variable.name = 'tenure', value.name = 'share')
  df[, tenure := str_replace_all(tenure, "share", "occupied")]

  return(df)
}

create_tenure_tract_map <- function(table, shape_tract, shape_place, title) {
  # Generate tract shape cut to place of interest and display in leaflet
  shape_tract_all <- left_join(shape_tract, table, by = c('geoid' = 'tract_geoid'))
  
  # Trim Tracts for current place
  shp_cut <- st_intersection(shape_tract_all, shape_place)

  # Filter for POC owner
  po <- shp_cut %>% filter(grouping == 'People of Color' & tenure == 'owner_occupied')
  # Filter for POC renter
  pr <- shp_cut %>% filter(grouping == 'People of Color' & tenure == 'renter_occupied')
  # Filter for Hispanic/Latino owner
  ho <- shp_cut %>% filter(grepl("^Hispanic", grouping) & tenure == 'owner_occupied')
  # Filter for Hispanic/Latino renter
  hr <- shp_cut %>% filter(grepl("^Hispanic", grouping) & tenure == 'renter_occupied')
  
  # Determine Bins
  rng <- range(shp_cut$share)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to) * round_to
  breaks <- (max_bin*c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1))
  bins <- c(0, breaks)
  
  pal <- colorBin("Purples", domain = shp_cut$share, bins = bins)
  
  title <- tags$div(HTML(title))
  labels <- paste0("<b>Tract ", shp_cut$geoid,"</b>", "<br>People of Color: ", label_percent(accuracy = 0.1, suffix = "%")(shp_cut$share)) %>%
    lapply(htmltools::HTML)
  
  ## Create Map ----
  
  shps <- list("POC Owner" = po, "POC Renter" = pr, "Hispanic/Latino Owner" = ho, "Hispanic/Latino Renter" = hr)
  
  m <- leaflet(data = shp_cut, options = leafletOptions(zoomControl=FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Place Boundary", names(shps)),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addPolygons(data = shape_place,
                fillColor = "76787A",
                weight = 4,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
                fillOpacity = 0.0,
                group = "Place Boundary") %>%
    addControl(title, position = "topleft")
  
  ### add layers ----
  
  for(s in 1:length(shps)){

    m <- m %>% 
      addPolygons(fillColor = pal(shps[[s]]$share),
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
                  group = names(shps)[s])
  }
  
  return(m)
  
}

shp <- tract.shape %>% 
  filter(census_year == 2010)

pl <- community.shape %>%
  filter(geog_name == 'Bellevue')

d <- create_tenure_tract_table()
 
y <- create_tenure_tract_map(table = d, shape_tract = shp, shape_place = pl, title = 'Test test')