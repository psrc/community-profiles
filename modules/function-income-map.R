# library(tidyverse)
# library(data.table)
# source('modules/function-query-sqlite-chas.R')

create_income_tract_table <- function() {
  # Generate tract level table for RDI Income metric. To be used in tract map.
  
  # gather T1 table
  chas_table <- 'T1'
  dfs <- gather_tables(juris = 'tract', chas_table)
  
  # Assemble Table ----
  # desc <- c('Extremely Low-Income (≤30% AMI)',
  #           'Very Low-Income (30-50%)',
  #           'Low-Income (50-80%)',
  #           'Moderate Income (80-100%)',
  #           'Above Median Income (>100%)')
  
  cols <- c('variable_name', 'sort','chas_year', 'tract_geoid', 'estimate', 'moe',  'tenure',
            'household_income', 'race_ethnicity', 'income_grp', 'race_ethnicity_grp')
  
  dfs$T1[, income_grp := fcase(household_income == 'All', 'All',
                                household_income == 'less than or equal to 30% of HAMFI', 'Less than 80% AMI',
                                household_income == 'greater than 30% but less than or equal to 50% of HAMFI', 'Less than 80% AMI',
                                household_income == 'greater than 50% but less than or equal to 80% of HAMFI', 'Less than 80% AMI',
                                household_income == 'greater than 80% but less than or equal to 100% of HAMFI', 'Greater than 80% AMI',
                                household_income == 'greater than 100% of HAMFI', 'Greater than 80% AMI')]
  
  dfs$T1[, race_ethnicity_grp := fcase(grepl("^American Indian ", race_ethnicity), "People of Color",
                                        grepl("^Asian ", race_ethnicity), "People of Color",
                                        grepl("^Black ", race_ethnicity), "People of Color",
                                        grepl("^Hispanic, any race", race_ethnicity), "Hispanic or Latino (of any race)",
                                        grepl("^Pacific ", race_ethnicity), "People of Color",
                                        grepl("^White ", race_ethnicity), "White",
                                        grepl("^All", race_ethnicity), "All Races")]
  
  # exclude high level totals
  df <- dfs$T1[!(sort %in% c(1, 2, 75)),]
  df <- df[, ..cols]
  
  # aggregate
  df_sum <- df[, . (estimate = sum(estimate)), by = c('chas_year', 'tract_geoid', 'tenure', 'income_grp', 'race_ethnicity_grp')]
  
  # create shares
  denom <- df_sum[income_grp != 'All', .(denom = sum(estimate)), by = c('chas_year', 'tract_geoid', 'tenure', 'race_ethnicity_grp')]
  
  df_join <- merge(df_sum, denom, by = c('chas_year', 'tract_geoid', 'tenure', 'race_ethnicity_grp'))
  
  df_join[, share := estimate/denom]
  df_join[is.nan(share), share := 0]
  
  return(df_join)
}

create_income_tract_map <- function(table, tenure_type = c("Owner", "Renter"), shape_tract, shape_place) {
  # Generate tract shape cut to place of interest and display in leaflet

  table <- table %>% 
    filter(grepl("^Less.*80.*", income_grp)) %>% 
    filter(grepl(paste0(tenure_type, ".*"), tenure)) %>% 
    replace_na(list(share = 0))
  
  shape_tract_all <- left_join(shape_tract, table, by = c('geoid' = 'tract_geoid'))

  # Trim Tracts for current place
  shape_place_valid <- st_make_valid(shape_place)
  shp_cut_ids <- st_intersection(shape_tract_all, shape_place_valid)
  shp_cut <- shape_tract_all %>% 
    filter(geoid %in% shp_cut_ids$geoid)
  
  p <- shp_cut %>% filter(race_ethnicity_grp == 'People of Color')
  # h <- shp_cut %>% filter(grepl("^Hispanic", race_ethnicity_grp))
  
  # Determine Bins
  rng <- range(shp_cut$share)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to) * round_to
  breaks <- (max_bin*c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1))
  bins <- c(0, breaks)
  
  pal <- colorBin("Purples", domain = shp_cut$share, bins = bins)
  
  ## Create Map ----
  
  title <- tags$div(HTML("POC ", tenure_type, " Households At or Below 80% AMI"))
  
  shps <- list("People of Color (POC)" = p#, 
               # "Hispanic/Latino" = h
               )
  
  m <- leaflet(data = shp_cut, options = leafletOptions(zoomControl=FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Place Boundary", names(shps)),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addControl(title, position = "topleft")
  
  ### add layers ----
  
  for(s in 1:length(shps)){
    labels <- paste0("<b>Tract ", shps[[s]]$geoid,"</b>", "<br>", shps[[s]]$race_ethnicity_grp, ", ",
                     str_to_lower(shps[[s]]$tenure),"<br>(Income ≤80%): ",
                     label_percent(accuracy = 0.1, suffix = "%")(shps[[s]]$share)) %>%
      lapply(htmltools::HTML)
    
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
  
  m <- m %>%
    addPolygons(data = shape_place,
                fillColor = "76787A",
                weight = 3,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
                fillOpacity = 0.0,
                group = "Place Boundary") %>%
    hideGroup(c(names(shps)[2]))
  
  return(m)
}

# shp <- tract.shape %>%
#   filter(census_year == 2010)
# 
# pl <- community.shape %>%
#   filter(geog_name == 'Bellevue')
# 
# x <- create_income_tract_table()
# 
# create_income_tract_map(table = x, tenure_type = "Renter", shape_tract = shp, shape_place = pl)
# create_income_tract_map(table = x, tenure_type = "Owner", shape_tract = shp, shape_place = pl)


