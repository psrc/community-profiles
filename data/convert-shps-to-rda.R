library(tidyverse)
library(sf)
library(data.table)

# Covert shapefiles to rda

jurisdictions <- as_tibble(fread('data//jurisdictions.csv'))

## jurisdictions ----

city.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/City_Boundaries/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |> 
  select(city_name, cnty_name) |> 
  mutate(cnty_name = paste0(cnty_name, " County")) |> 
  mutate(city_name = gsub("Sea Tac", "SeaTac", city_name)) |> 
  mutate(city_name = gsub("Beaux Arts", "Beaux Arts Village", city_name)) |> 
  rename(geog_name = city_name, county = cnty_name)

rgeo.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Geographies/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |> 
  filter(juris == "Silverdale UGA") |> 
  mutate(juris = str_replace(juris, "Silverdale UGA", "Silverdale")) |> 
  select(geog_name = juris, county = cnty_name) |> 
  mutate(county =paste0(county, " County"))

county.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/County_Boundaries/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |> 
  filter(psrc == 1) |> 
  select(geog_name = county_nm) |> 
  mutate(geog_name = paste0(geog_name, " County"), county = geog_name)

community.shape <- rbind(city.shape, county.shape, rgeo.shape)
community.shape <- left_join(community.shape, jurisdictions, by = c("geog_name" = "juris_name"))
rm(city.shape, county.shape, rgeo.shape)

community.point <- community.shape %>% st_drop_geometry()

## tracts ----

tract.2010 <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Census_Tracts_2010/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |>  
  select(geoid = geoid10, county = county_name) |> 
  mutate(county = paste0(county, " County"), census_year = 2010)

tract.2020 <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Census_Tracts_2020/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |>  
  select(geoid = geoid20, county = county_name) |> 
  mutate(county = paste0(county, " County"), census_year = 2020)

tract.shape <- rbind(tract.2010, tract.2020)
rm(tract.2010, tract.2020)

## Transportation ----

rtp.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Capacity_Projects/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |> 
  select(ID = MTP_ID, Sponsor, Type = Agency_Typ, Title = Project_Ti, Improvement = Type, Completion, Status, Cost = Total_Cost) |>
  mutate(Cost = as.numeric(Cost), Completion = as.character(Completion))

tip.shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/24_06_TIP_gdb/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |>
  select(ID = ProjNo, Sponsor = PlaceShort, Title = ProjectTit, Improvement = Impr_type, Completion = EstComplet, Cost = TotCost) |>
  mutate(Cost = as.numeric(Cost), Completion = as.character(Completion), Status = "2023-2026 TIP") |>
  mutate(Type = case_when(
    Sponsor %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County") ~ "Counties",
    Sponsor %in% c("Kitsap Transit", "Pierce Transit", "Everett Transit", "King County Metro", "Community Transit") ~ "Local Transit",
    Sponsor %in% c("Sound Transit") ~ "Regional Transit",
    Sponsor %in% c("Tulalip Tribes", "Port of Seattle", "Muckleshoot Indian Tribe", "Port of Everett") ~ "Ports/Tribes",
    Sponsor %in% c("WSDOT") ~ "State",
    Sponsor %in% c("Washington State Ferries") ~ "WSF")) |>
  mutate(Type = replace_na(Type, "Cities")) |> 
  relocate(Type, .after = Sponsor) |> 
  relocate(Status, .after = Completion)

projects.shape <- rbind(tip.shape, rtp.shape)

## Displacement Risk ----

disp_risk_path <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Displacement_Risk_Data/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
disprisk.shape <- st_read(disp_risk_path)

## Export to data subdirectory & X:/DSA/shiny-uploads/community-profiles/data ----

save(community.point, community.shape, tract.shape, rtp.shape, tip.shape, projects.shape, disprisk.shape, file = 'data/community_profile_shapes.rda')

# file.copy(from = 'data/community_profile_shapes.rda', to = "X:/DSA/shiny-uploads/community-profiles/data/community_profile_shapes.rda", overwrite = TRUE)
