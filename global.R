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

load(file.path('data', 'community_profile_shapes.rda'))

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
