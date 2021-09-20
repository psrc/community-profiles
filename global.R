library(DT)

library(shiny)
library(shinyjs)
library(shinyBS)

library(ggplot2)
library(scales)
library(plotly)
library(foreign)

library(leaflet)
library(sf)

library(tidycensus)
library(tidyverse)
library(data.table)

library(here)

source('functions.R')

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285 

yrs <- seq(2017,2019,1)
download.new.census.data <- 'no'
tables.for.profiles <- c('DP02','DP03','DP04','DP05','B08303')

dp02.variables.pre19 <- c("DP02_0015","DP02_0071","DP02_0073","DP02_0075","DP02_0077","DP02_0086")
dp02.variables.post19 <- c("DP02_0016","DP02_0072","DP02_0074","DP02_0076","DP02_0078","DP02_0087")

dp03.variables.pre19 <- c("DP03_0009","DP03_0018","DP03_0019","DP03_0020","DP03_0021","DP03_0022","DP03_0023","DP03_0024", 
                          "DP03_0025","DP03_0026","DP03_0027","DP03_0028","DP03_0029","DP03_0030","DP03_0031","DP03_0032",
                          "DP03_0033","DP03_0034","DP03_0035","DP03_0036","DP03_0037","DP03_0038","DP03_0039","DP03_0040",
                          "DP03_0041","DP03_0042","DP03_0043","DP03_0044","DP03_0045",
                          "DP03_0051","DP03_0052","DP03_0053","DP03_0054","DP03_0055","DP03_0056","DP03_0057","DP03_0058",
                          "DP03_0059","DP03_0060","DP03_0061","DP03_0062")
dp03.variables.post19 <- c("DP03_0009","DP03_0018","DP03_0019","DP03_0020","DP03_0021","DP03_0022","DP03_0023","DP03_0024", 
                          "DP03_0025","DP03_0026","DP03_0027","DP03_0028","DP03_0029","DP03_0030","DP03_0031","DP03_0032",
                          "DP03_0033","DP03_0034","DP03_0035","DP03_0036","DP03_0037","DP03_0038","DP03_0039","DP03_0040",
                          "DP03_0041","DP03_0042","DP03_0043","DP03_0044","DP03_0045",
                          "DP03_0051","DP03_0052","DP03_0053","DP03_0054","DP03_0055","DP03_0056","DP03_0057","DP03_0058",
                          "DP03_0059","DP03_0060","DP03_0061","DP03_0062")

dp04.variables.pre19 <- c("DP04_0006","DP04_0007","DP04_0008","DP04_0009","DP04_0010","DP04_0011","DP04_0012","DP04_0013","DP04_0014",
                          "DP04_0057","DP04_0058","DP04_0059","DP04_0060","DP04_0061",
                          "DP04_0080","DP04_0081","DP04_0082","DP04_0083","DP04_0084","DP04_0085","DP04_0086","DP04_0087","DP04_0088","DP04_0089",
                          "DP04_0126","DP04_0127","DP04_0128","DP04_0129","DP04_0130","DP04_0131","DP04_0132","DP04_0133","DP04_0134")
dp04.variables.post19 <- c("DP04_0006","DP04_0007","DP04_0008","DP04_0009","DP04_0010","DP04_0011","DP04_0012","DP04_0013","DP04_0014",
                          "DP04_0057","DP04_0058","DP04_0059","DP04_0060","DP04_0061",
                          "DP04_0080","DP04_0081","DP04_0082","DP04_0083","DP04_0084","DP04_0085","DP04_0086","DP04_0087","DP04_0088","DP04_0089",
                          "DP04_0126","DP04_0127","DP04_0128","DP04_0129","DP04_0130","DP04_0131","DP04_0132","DP04_0133","DP04_0134")

dp05.variables.pre19 <- c("DP05_0001","DP05_0005","DP05_0006","DP05_0007","DP05_0008","DP05_0009",
                          "DP05_0010","DP05_0011","DP05_0012","DP05_0013","DP05_0014","DP05_0015","DP05_0016","DP05_0017","DP05_0018",
                          "DP05_0063","DP05_0064","DP05_0065","DP05_0066","DP05_0067","DP05_0068","DP05_0069")
dp05.variables.post19 <- c("DP05_0001","DP05_0005","DP05_0006","DP05_0007","DP05_0008","DP05_0009",
                          "DP05_0010","DP05_0011","DP05_0012","DP05_0013","DP05_0014","DP05_0015","DP05_0016","DP05_0017","DP05_0018",
                          "DP05_0063","DP05_0064","DP05_0065","DP05_0066","DP05_0067","DP05_0068","DP05_0069")

final_tract_variables <- c("DP02_0071","DP02_0073","DP02_0075","DP02_0077",
                           "DP02_0072","DP02_0074","DP02_0076","DP02_0078",
                           "DP03_0019","DP03_0020","DP03_0021","DP03_0022","DP03_0023","DP03_0024", "DP03_0025", 
                           "DP03_0026","DP03_0027","DP03_0028","DP03_0029","DP03_0030","DP03_0031", 
                           "DP03_0032","DP03_0033","DP03_0034","DP03_0035","DP03_0036","DP03_0037","DP03_0038","DP03_0039","DP03_0040",
                           "DP03_0041","DP03_0042","DP03_0043","DP03_0044","DP03_0045","DP03_0062",
                           "DP04_0006","DP04_0007","DP04_0008","DP04_0009","DP04_0010","DP04_0011","DP04_0012","DP04_0013","DP04_0014",
                           "DP04_0058","DP04_0089","DP04_0134",
                           "DP05_0018","DP05_0064","DP05_0065","DP05_0066","DP05_0067","DP05_0068","DP05_0069")

outside.counties <- c("53057","53009","53007","53029","53031","53045","53037","53067","53067","53067")

# Shapefiles --------------------------------------------------------------
community.shape <- st_read('data//places_no_water_wgs1984.shp') %>% 
  st_transform(wgs84) %>%
  mutate(ZOOM = as.integer(ZOOM))

community.point <- community.shape %>% st_drop_geometry()
region.geoids <- community.point %>% filter(!(GEOID %in% outside.counties)) %>% select(GEOID) %>% distinct() %>% pull()

tract.shape <- st_read('data//extended_tract_2010_no_water_wgs1984.shp') %>% 
  st_transform(wgs84) %>%
  select(GEOID10)

# Census Data -------------------------------------------------------
if (download.new.census.data == 'yes') {
  census_data <- NULL
  
  for (y in yrs) {
    table.labels <- load_variables(year=y, dataset="acs5", cache = TRUE)
    dp.labels <- load_variables(year=y, dataset="acs5/profile", cache = TRUE)
    labels <- bind_rows(table.labels,dp.labels)
    
    for (t in tables.for.profiles) {
    
      p <- get_acs(geography="place",state="WA", year=y, table=t, survey='acs5', cache_table=TRUE) %>%
        mutate(census_year = y, place_type="pl", acs_type="acs5")
      
      percent.variables <- p %>% 
        filter(endsWith(variable,"P")) %>% 
        select(GEOID,variable,census_year,estimate,moe) %>%
        rename(estimate_percent=estimate, moe_percent=moe) %>%
        mutate(variable = str_sub(variable,1,nchar(variable)-1))
      
      p <- left_join(p,percent.variables, by=c("GEOID","variable","census_year")) %>%
        filter(!(endsWith(variable,"P")))
      
      c <- get_acs(geography="county",state="WA", counties = c("King","Kitsap","Pierce","Snohomish"), year=y, table=t, survey='acs5', cache_table=TRUE) %>%
        mutate(census_year = y, place_type="co", acs_type="acs5") 
      
      percent.variables <- c %>% 
        filter(endsWith(variable,"P")) %>% 
        select(GEOID,variable,census_year,estimate,moe) %>%
        rename(estimate_percent=estimate, moe_percent=moe) %>%
        mutate(variable = str_sub(variable,1,nchar(variable)-1))
      
      c <- left_join(c,percent.variables, by=c("GEOID","variable","census_year")) %>%
        filter(!(endsWith(variable,"P")))
      
      t.tbl <- get_acs(geography="tract", state="WA", counties = c("King","Kitsap","Pierce","Snohomish"), year=y, table=t, survey='acs5', cache_table=TRUE) %>%
        mutate(census_year = y, place_type="tr", acs_type="acs5")
        
      percent.variables <- t.tbl %>% 
        filter(endsWith(variable,"P")) %>% 
        select(GEOID,variable,census_year,estimate,moe) %>%
        rename(estimate_percent=estimate, moe_percent=moe) %>%
        mutate(variable = str_sub(variable,1,nchar(variable)-1))
      
      t.tbl <- left_join(t.tbl,percent.variables, by=c("GEOID","variable","census_year")) %>%
        filter(!(endsWith(variable,"P"))) %>% 
        filter(variable %in% final_tract_variables)
      
      d <- bind_rows(list(p,c,t.tbl))
      d <- left_join(d,labels, by=c("variable"="name"))
      
      if (t== 'DP02') {
        if (y < 2019) {
          keep.vars <- dp02.variables.pre19
          d <- d %>% filter(variable %in% keep.vars)
        } else {
          keep.vars <- dp02.variables.post19
          d <- d %>% filter(variable %in% keep.vars)
        }
      }
      
      if (t== 'DP03') {
        if (y < 2019) {
          keep.vars <- dp03.variables.pre19
          d <- d %>% filter(variable %in% keep.vars)
        } else {
          keep.vars <- dp03.variables.post19
          d <- d %>% filter(variable %in% keep.vars)
        }
      }
      
      if (t== 'DP04') {
        if (y < 2019) {
          keep.vars <- dp04.variables.pre19
          d <- d %>% filter(variable %in% keep.vars)
        } else {
          keep.vars <- dp04.variables.post19
          d <- d %>% filter(variable %in% keep.vars)
        }
      }
      
      if (t== 'DP05') {
        if (y < 2019) {
          keep.vars <- dp05.variables.pre19
          d <- d %>% filter(variable %in% keep.vars)
        } else {
          keep.vars <- dp05.variables.post19
          d <- d %>% filter(variable %in% keep.vars)
        }
      }
    
      ifelse(is.null(census_data), census_data <- d, census_data <- bind_rows(census_data,d))
      rm(p, c, t.tbl, d)
    } # end of tables loop
  } # end of year loop
  
  tract_data <- census_data %>% 
    filter(place_type %in% c("tr")) %>%
    separate(NAME, into=c("NAME","county", "state"),sep=",") %>%
    mutate(county = trimws(county, "l")) %>%
    filter(county %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County")) %>%
    select(-county, -state)
  
  census_data <- census_data %>% filter(GEOID %in% region.geoids)
  
  census_data <- bind_rows(census_data, tract_data)
  
  census_data <- census_data %>%
    rename(geoid=GEOID, margin_of_error=moe, geog_name=NAME, variable_name=variable, variable_description=label) %>%
    mutate(place_type = str_trim(place_type, "right")) %>%
    mutate(geog_name = gsub(", Washington", "", geog_name)) %>%
    filter(!(endsWith(geog_name,"CDP"))) %>%
    filter(!(endsWith(geog_name,"CDP (Pierce County)"))) %>%
    filter(!(endsWith(geog_name,"CDP (King County)"))) %>%
    mutate(geog_name = gsub(" city", "", geog_name)) %>%
    mutate(geog_name = gsub(" town", "", geog_name)) %>%
    mutate(geog_name = str_trim(geog_name, "right")) %>%
    select(-concept) %>%
    mutate(variable_description = str_extract(variable_description, "[^!!]*$")) %>%
    mutate(variable_description = case_when(
      variable_name == "DP04_0013" ~ "20+ Units",
      !(variable_name == "DP04_0013") ~ variable_description))
  
  fwrite(census_data, 'data//census_data_by_place.csv')

} else {
  census_data <- as_tibble(fread('data//census_data_by_place.csv'))
}

# Mode Share Information --------------------------------------------------
ms_var <- c("DP03_0018","DP03_0019","DP03_0020","DP03_0021","DP03_0022","DP03_0023","DP03_0024")
ms_cols <- c("variable_description","estimate","margin_of_error")
ms_total <- c("Workers 16 years and over")
ms_remove <- NULL
ms_order <- c("Car, truck, or van -- drove alone", "Car, truck, or van -- carpooled", "Public transportation (excluding taxicab)", 
              "Walked", "Other means", "Worked at home", "Worked from home")

numeric_ms <- c("estimate","margin_of_error")
percent_ms <- c("Share","Region")
mode_length <- 10

# Travel Time Information -------------------------------------------------
tt_var <- c("B08303_001","B08303_002","B08303_003","B08303_004","B08303_005","B08303_006","B08303_007","B08303_008","B08303_009","B08303_010","B08303_011","B08303_012","B08303_013")
tt_cols <- c("variable_description","estimate","margin_of_error")
tt_total <- c("Total","Total:")
tt_remove <- NULL
tt_order <- c("Less than 5 minutes", "5 to 9 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes","25 to 29 minutes","30 to 34 minutes","35 to 39 minutes","40 to 44 minutes","45 to 59 minutes","60 to 89 minutes","90 or more minutes")

numeric_tt <- c("estimate","margin_of_error")
percent_tt <- c("Share","Region")
tt_length <- 15

# Occupation, Industry and Income Information -----------------------------
occ_var <- c("DP03_0026","DP03_0027","DP03_0028","DP03_0029","DP03_0030","DP03_0031")
occ_cols <- c("variable_description","estimate","margin_of_error")
occ_total <- c("Civilian employed population 16 years and over")
occ_remove <- NULL
occ_order <- c("Natural resources, construction, and maintenance occupations","Management, business, science, and arts occupations", 
               "Production, transportation, and material moving occupations", "Sales and office occupations", "Service occupations")

ind_var <- c("DP03_0032","DP03_0033","DP03_0034","DP03_0035","DP03_0036","DP03_0037","DP03_0038","DP03_0039","DP03_0040","DP03_0041","DP03_0042","DP03_0043","DP03_0044","DP03_0045")
ind_cols <- c("variable_description","estimate","margin_of_error")
ind_total <- c("Civilian employed population 16 years and over")
ind_remove <- NULL
ind_order <- c("Agriculture, forestry, fishing and hunting, and mining", "Construction", "Educational services, and health care and social assistance", 
               "Arts, entertainment, and recreation, and accommodation and food services", 
               "Finance and insurance, and real estate and rental and leasing", "Information", 
               "Manufacturing", "Other services, except public administration", 
               "Professional, scientific, and management, and administrative and waste management services", 
               "Public administration", "Retail trade","Transportation and warehousing, and utilities","Wholesale trade")

inc_var <- c("DP03_0051","DP03_0052","DP03_0053","DP03_0054","DP03_0055","DP03_0056","DP03_0057","DP03_0058","DP03_0059","DP03_0060","DP03_0061")
inc_cols <- c("variable_description","estimate","margin_of_error")
inc_total <- c("Total households")
inc_remove <- NULL
inc_order <- c("Less than $10,000","$10,000 to $14,999","$15,000 to $24,999","$25,000 to $34,999","$35,000 to $49,999",
               "$50,000 to $74,999","$75,000 to $99,999","$100,000 to $149,999","$150,000 to $199,999","$200,000 or more")

numeric_jobs <- c("estimate","margin_of_error")
percent_jobs <- c("Share","Region")
job_length <- 15

# Housing Information -----------------------------------------------------
hs_var <- c("DP04_0006","DP04_0007","DP04_0008","DP04_0009","DP04_0010","DP04_0011","DP04_0012","DP04_0013","DP04_0014")
hs_cols <- c("variable_description","estimate","margin_of_error")
hs_total <- c("Total housing units")
hs_remove <- NULL
hs_order <- c("1-unit, detached", "1-unit, attached", "2 units", "3 or 4 units", "5 to 9 units", "10 to 19 units", "20+ Units", "Mobile home")

numeric_hs <- c("estimate","margin_of_error")
percent_hs <- c("Share","Region")
house_length <- 10

# Vehicle Availability Information ----------------------------------------------------
va_var <- c("DP04_0057","DP04_0058","DP04_0059","DP04_0060","DP04_0061")
va_cols <- c("variable_description","estimate","margin_of_error")
va_total <- c("Occupied housing units")
va_remove <- NULL
va_order <- c("No vehicles available", "1 vehicle available", "2 vehicles available", "3 or more vehicles available")

numeric_va <- c("estimate","margin_of_error")
percent_va <- c("Share","Region")
va_length <- 5

# Home Value Information --------------------------------------------------
hv_var <- c("DP04_0080","DP04_0081","DP04_0082","DP04_0083","DP04_0084","DP04_0085","DP04_0086","DP04_0087","DP04_0088")
hv_cols <- c("variable_description","estimate","margin_of_error")
hv_total <- c("Owner-occupied units")
hv_remove <- NULL
hv_order <- c("Less than $50,000", "$50,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 to $299,999", "$300,000 to $499,999", 
              "$500,000 to $999,999", "$1,000,000 or more")

numeric_hv <- c("estimate","margin_of_error")
percent_hv <- c("Share","Region")
hv_length <- 10

# Rent Information --------------------------------------------------------
rent_var <- c("DP04_0126","DP04_0127","DP04_0128","DP04_0129","DP04_0130","DP04_0131","DP04_0132","DP04_0133")
rent_cols <- c("variable_description","estimate","margin_of_error")
rent_total <- c("Occupied units paying rent")
rent_remove <- NULL
rent_order <- c("Less than $500", "$500 to $999", "$1,000 to $1,499", "$1,500 to $1,999", "$2,000 to $2,499", "$2,500 to $2,999", "$3,000 or more")

numeric_rent <- c("estimate","margin_of_error")
percent_rent <- c("Share","Region")
rent_length <- 10

# Age Information ---------------------------------------------------------
age_var <- c("DP05_0001","DP05_0005","DP05_0006","DP05_0007","DP05_0008","DP05_0009","DP05_0010","DP05_0011","DP05_0012","DP05_0013","DP05_0014","DP05_0015","DP05_0016","DP05_0017")
age_cols <- c("variable_description","estimate","margin_of_error")
age_total <- c("Total population")
age_remove <- NULL
age_order <- c("Under 5 years","5 to 9 years","10 to 14 years","15 to 19 years","20 to 24 years","25 to 34 years",
               "35 to 44 years","45 to 54 years","55 to 59 years","60 to 64 years","65 to 74 years","75 to 84 years", "85 years and over")

numeric_age <- c("estimate","margin_of_error")
percent_age <- c("Share","Region")
age_length <- 15

# Race Information --------------------------------------------------------
race_var <- c("DP05_0063","DP05_0064","DP05_0065","DP05_0066","DP05_0067","DP05_0068","DP05_0069")
race_cols <- c("variable_description","estimate","margin_of_error")
race_total <- c("Total population")
race_remove <- NULL
race_order <- c("White","Black or African American","American Indian and Alaska Native","Asian","Native Hawaiian and Other Pacific Islander","Some other race")

numeric_race <- c("estimate","margin_of_error")
percent_race <- c("Share","Region")
race_length <- 10

# Disability Information --------------------------------------------------
disability_var <- c("DP02_0077","DP02_0078")
disability_cols <- c("variable_description","estimate","margin_of_error")
disability_total <- c("Total Population")
disability_remove <- NULL
disability_order <- c("With a disability")

numeric_disability <- c("estimate","margin_of_error")
percent_disability <- c("Share","Region")
disability_length <- 5

# Dropdown List Creations -------------------------------------------------
data_modes <- census_data %>% 
  filter(variable_name %in% c("DP03_0019","DP03_0020","DP03_0021","DP03_0022","DP03_0023","DP03_0024")) %>%
  select(variable_description) %>%
  distinct() %>%
  pull()

data_race <- census_data %>% 
  filter(variable_name %in% c("DP05_0064","DP05_0065","DP05_0066","DP05_0067","DP05_0068","DP05_0069")) %>%
  select(variable_description) %>%
  distinct() %>%
  pull()

data_disability <- census_data %>% 
  filter(variable_name %in% c("DP02_0073","DP02_0074","DP02_0075","DP02_0076","DP02_0077","DP02_0078","DP02_0071","DP02_0072")) %>%
  select(variable_description) %>%
  distinct() %>%
  pull()

data_years <- census_data %>%
  select(census_year) %>%
  distinct() %>%
  pull()

data_places <- census_data %>%
  filter(place_type %in% c("pl","co")) %>%
  select(geog_name) %>%
  distinct() %>%
  pull()

data_hu <- census_data %>% 
  filter(variable_name %in% c("DP04_0007","DP04_0008","DP04_0009","DP04_0010","DP04_0011","DP04_0012","DP04_0013","DP04_0014")) %>%
  select(variable_description) %>%
  distinct() %>%
  pull()

data_occ <- census_data %>% 
  filter(variable_name %in% c("DP03_0027","DP03_0028","DP03_0029","DP03_0030","DP03_0031")) %>%
  select(variable_description) %>%
  distinct() %>%
  pull()

data_ind <- census_data %>% 
  filter(variable_name %in% c("DP03_0033","DP03_0034","DP03_0035","DP03_0036","DP03_0037","DP03_0038","DP03_0039","DP03_0040","DP03_0041","DP03_0042","DP03_0043","DP03_0044","DP03_0045")) %>%
  select(variable_description) %>%
  distinct() %>%
  pull()

