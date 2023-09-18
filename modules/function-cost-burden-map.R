# library(tidyverse)
# library(data.table)
# source('modules/function-query-sqlite-chas.R')

create_cost_burden_tract_table <- function() {
  # Generate tract level table for RDI Cost Burden metric. To be used in tract map.

  chas_tables <- 'T9'
  dfs <- gather_tables(juris = 'tract', chas_tables)
  df <- dfs$T9[!(sort %in% c(1, 2, 38))]
  
  # Assemble Table ----
  
  desc <- c("less than or equal to 30%",
            "greater than 30% but less than or equal to 50%",
            "greater than 50%",
            "not computed (no/negative income)",
            "All")
  
  names(desc) <- c("No Cost Burden", "Cost-Burdened (30-50%)", "Severely Cost-Burdened (>50%)", "Not Calculated", "All")
  
  # pivot wider
  df[, cost_burden_grp := fcase(cost_burden == "less than or equal to 30%", "No Cost Burden",
                                cost_burden == "greater than 30% but less than or equal to 50%", "Cost-Burdened (>30%)",
                                cost_burden == "greater than 50%", "Cost-Burdened (>30%)",
                                cost_burden ==  "not computed (no/negative income)", "Not Calculated",
                                cost_burden == "All", "All")]
  
  df[, race_ethnicity_grp := fcase(grepl("^American Indian ", race_ethnicity), "People of Color",
                                   grepl("^Asian ", race_ethnicity), "People of Color",
                                   grepl("^Black ", race_ethnicity), "People of Color",
                                   grepl("^Hispanic, any race", race_ethnicity), "People of Color",
                                   grepl("^other ", race_ethnicity), "People of Color",
                                   grepl("^Pacific ", race_ethnicity), "People of Color",
                                   grepl("^White ", race_ethnicity), "White",
                                   grepl("^All", race_ethnicity), "All")]
  
  df <- df[, .(estimate = sum(estimate)), by = c('tract_geoid', 'chas_year', 'tenure', 'cost_burden_grp', 'race_ethnicity_grp')]
  
  # add denominator column
  df_denom <- df[cost_burden_grp == 'All', 
                 .(tract_geoid, chas_year, tenure, race_ethnicity_grp, estimate_denom = estimate)]
  
  df <- merge(df, df_denom, by = c('tract_geoid', 'chas_year', 'tenure', 'race_ethnicity_grp'))  
  
  # calculate shares
  df[, share := estimate/estimate_denom]

  return(df)
}

create_cost_burden_tract_map <- function(table, tenure_type = c("Owner", "Renter"), shape_tract, shape_place) {
  # Generate tract shape cut to place of interest and display in leaflet
  
  table <- table %>% 
    filter(grepl(".*30.*", cost_burden_grp)) %>% 
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

  title <- tags$div(HTML("Cost-Burdened POC ", tenure_type, " Households"))

  shps <- list("People of Color (POC)" = p
               # , "Hispanic/Latino" = h
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
                     str_to_lower(shps[[s]]$tenure),"<br>(cost-burden >30%): ",
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
                    fillOpacity = 0.7#,
                    # bringToFront = TRUE
                    ),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  group = names(shps)[s])
  }

  m <- m %>%
    addPolygons(data = shape_place,
                fill = FALSE,
                weight = 3,
                opacity = 1.0,
                color = "#91268F",
                dashArray = "4",
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
# x <- create_cost_burden_tract_table()
# 
# create_cost_burden_tract_map(table = x, tenure_type = "Renter", shape_tract = shp, shape_place = pl)
