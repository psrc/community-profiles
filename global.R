library(DT)

library(shiny)
library(shinyjs)
library(shinyBS)

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

disprisk.shape <- read_rds(file.path('data', "disp_risk_shp.rds"))

numeric_variables <- c("Estimate","MoE")
percent_variables <- c("Share","Region")
final.nms <- c("ID","Sponsor","Type","Title","Improvement","Completion","Status","Cost")
currency.rtp <- c("Cost")
rtp.status <- c("Approved","Conditionally Approved","ROW Conditionally Approved","Candidate", "Financially Constrained", "Unprogrammed")


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
