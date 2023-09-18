# tract-based script to consolidate information to display on leaflet map for Rental Affordability metric

# library(tidyverse)
# library(data.table)
# source('modules/function-query-chas.R')

create_rental_affordability_tract_table <- function() {
  # Generate tract level table for rental affordability metric. To be used in tract map.
  
  chas_tables <- c('T8', 'T15C', 'T14B')
  dfs <- gather_tables(juris = 'tract', chas_tables)
  
  # share of rental units < 80% AMI ----
  
  desc <- c('Extremely Low Income (<30% AMI)',
            'Very Low Income (30-50% AMI)',
            'Low Income (50-80% AMI)',
            'Moderate Income (80-100% AMI)',
            'Greater than 100% of AMI', # fyi: not necessary but missing from tables below 
            'All')
  
  # Table 15C
  t15c_head <- c(4, 25, 46, 67, 3)
  t15c_desc <- c(desc[1:4], desc[6])
  names(t15c_head) <- t15c_desc
  t15c <- dfs$T15C[sort %in% t15c_head, ]
  t15c <- t15c[, `:=` (sort = factor(sort, levels = t15c_head), col_desc = 'rental_unit_affordability')][order(sort)]
  t15c$description <- names(t15c_head)[t15c$sort]
  
  # Table 14B
  t14b_head <- c(4, 8, 12, 16, 3)
  t14b_desc <- c(desc[1:4], desc[6])
  names(t14b_head) <- t14b_desc
  t14b <- dfs$T14B[sort %in% t14b_head, ]
  t14b <- t14b[, `:=` (sort = factor(sort, levels = t14b_head), col_desc = 'vacant_rental_units')][order(sort)]
  t14b$description <- names(t14b_head)[t14b$sort]
  
  cols <- c('variable_name', 'sort','chas_year', 'tract_geoid', 'estimate', 'moe',  'col_desc', 'description')
  ra_dfs <- map(list(t15c, t14b), ~.x[, ..cols])
  df <- rbindlist(ra_dfs)

  df_sum <- df[, . (estimate = sum(estimate)), by = c('chas_year', 'tract_geoid', 'description')
  ][,  grouping := fcase(description == 'All', 'All',
                         description == 'Extremely Low Income (<30% AMI)','Less than 80% AMI',
                         description == 'Very Low Income (30-50% AMI)', 'Less than 80% AMI',
                         description == 'Low Income (50-80% AMI)', 'Less than 80% AMI',
                         description == 'Moderate Income (80-100% AMI)', 'Greater than 80% AMI')]
  
  df_sum <- df_sum[, .(estimate = sum(estimate)), by = c('chas_year', 'tract_geoid', 'grouping')]

  df_denom <- df_sum[grouping == 'All', .(tract_geoid, denom = estimate)]
  
  df_calc <- merge(df_sum, df_denom, by = 'tract_geoid')
  
  df_calc[, share := estimate/denom]
  
  d <- df_calc[grouping %in% str_subset(grouping, "Less.*"), .(chas_year, tract_geoid, grouping, share)]
}

create_chas_tract_map <- function(shape_tract, shape_place, title) {
  # Generate tract shape cut to place of interest and display in leaflet

  # Trim Tracts for current place
  shape_place_valid <- st_make_valid(shape_place)
  shp_cut_ids <- st_intersection(shape_tract, shape_place_valid)
  shp_cut <- shape_tract %>% 
    filter(geoid %in% shp_cut_ids$geoid)
  
  # replace Nan with 0
  shp_cut$share <- shp_cut$share %>% replace_na(0)
  
  # Determine Bins
  rng <- range(shp_cut$share)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to) * round_to
  breaks <- (max_bin*c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1, 1.5))
  bins <- c(0, breaks)
  
  pal <- colorBin("Greens", domain = shp_cut$share, bins = bins)
  
  title <- tags$div(HTML(title))
  labels <- paste0("<b>Tract ", shp_cut$geoid,"</b>", "<br>< 80% AMI: ", label_percent(accuracy = 0.1, suffix = "%")(shp_cut$share)) %>%
    lapply(htmltools::HTML)
  
  ## Create Map ----
  
  m <- leaflet(data = shp_cut, options = leafletOptions(zoomControl=FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Place Boundary", "Census Tracts"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addPolygons(fillColor = pal(shp_cut$share),
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
    addPolygons(data = shape_place,
                fill = FALSE,
                weight = 3,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
                group = "Place Boundary")%>%
    addControl(title, position = "topleft")
  
  return(m)
}

# shp <- tract.shape %>%
#   filter(census_year == 2010)
# 
# pl <- community.shape %>%
#   filter(geog_name == 'Snohomish County')
# 
# 
# d <- create_rental_affordability_tract_table()
# 
# s <- shp %>%
#   left_join(d, by = c('geoid' = 'tract_geoid'))
# 
# create_chas_tract_map(shape_tract = s, shape_place = pl, title = 'Something')

