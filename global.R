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

library(odbc)
library(DBI)

source('functions.R')


# Database Connections ----------------------------------------------------
server.name <- "AWS-PROD-SQL\\SOCKEYE"
database.name <- "Elmer"
jurisdiction.tbl <- "Political.jurisdiction_dims"
ofm.facts.tbl <- "ofm.april_1_estimate_facts"
ofm.juris.tbl <- "ofm.jurisdiction_dim"
ofm.pubs.tbl <- "ofm.publication_dim"

db.con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server.name,
                    database = database.name,
                    UID = Sys.getenv("userid"),
                    PWD = Sys.getenv("pwd")
)

# Jurisdiction Data -------------------------------------------------------
airport.community <- c("Auburn", "Burien", "Covington", "Des Moines", "Federal Way", "Kenmore", "Kent", "Lake Forest Park",
                       "Normandy Park", "Renton", "SeaTac", "Seattle", "Skykomish", "Tukwila", "Bremerton", "Eatonville",
                       "Gig Harbor", "Lakewood", "Steilacoom", "Tacoma", "University Place", "Arlington", "Darrington",
                       "Everett", "Marysville", "Monroe","Mukilteo", "Snohomish", "Sultan")

airport.community <- enframe(airport.community) %>%
  mutate(airport_affected = "Yes") %>%
  select(-name)

jurisdictions <- as_tibble(dbReadTable(db.con,SQL(jurisdiction.tbl))) %>%
  mutate(juris_name = gsub("Seatac","SeaTac",juris_name)) %>%
  mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",juris_name)) %>%
  select(juris_name, regional_geography, airport_affected) %>%
  distinct() %>%
  mutate(regional_geography=gsub("HCT","High Capacity Transit Community",regional_geography)) %>%
  mutate(regional_geography=gsub("Metro","Metropolitan Cities",regional_geography)) %>%
  mutate(regional_geography=gsub("Core","Core Cities",regional_geography)) %>%
  mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",regional_geography)) %>%
  select(-airport_affected)

jurisdictions <- left_join(jurisdictions, airport.community, by=c("juris_name"="value")) %>%
  mutate(across(airport_affected, ~replace_na(.x, "No")))

# OFM Population Data -----------------------------------------------------
ofm.pop <- as_tibble(dbReadTable(db.con,SQL(ofm.facts.tbl)))

# Add Jurisdiction Details
temp <- as_tibble(dbReadTable(db.con,SQL(ofm.juris.tbl)))
ofm.pop <- left_join(ofm.pop, temp, by=c("jurisdiction_dim_id"))

# Add Publication Details
temp <- as_tibble(dbReadTable(db.con,SQL(ofm.pubs.tbl)))
ofm.pop <- left_join(ofm.pop, temp, by=c("publication_dim_id"))

ofm.pub.yr <- ofm.pop %>% select(publication_year) %>% distinct() %>% pull() %>% max()
ofm.pop <- ofm.pop %>% 
  filter(publication_year==ofm.pub.yr) %>%
  select(jurisdiction_name, total_population, housing_units, estimate_year) %>%
  mutate(jurisdiction_name= str_replace(jurisdiction_name, " \\(part\\)", "")) %>%
  group_by(jurisdiction_name, estimate_year) %>%
  summarize(total_population=sum(total_population), housing_units=sum(housing_units))

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

rtp.url <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/RTP/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson"
rtp.shape <- st_read(rtp.url) %>%
  mutate(ImprovementType=" ") %>%
  mutate(TotalCost = gsub(",","",TotalCost)) %>%
  mutate(TotalCost = as.numeric(TotalCost))

tip.url <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/TIP_19_22/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson"
tip.shape <- st_read(tip.url) %>%
  mutate(TotCost = as.numeric(TotCost)) %>%
  mutate(EstCompletionYear = as.numeric(EstCompletionYear)) %>%
  mutate(Status="2019-2022 TIP")

rtp.cols <- c("mtpid","Sponsor","Title","ImprovementType","CompletionYear","MTPStatus","TotalCost")
tip.cols <- c("ProjNo","PlaceShortName","ProjectTitle","ImproveType","EstCompletionYear","Status","TotCost")
final.nms <- c("ID","Sponsor","Title","Improvement Type","Project Completion","Project Status","Cost")

currency.rtp <- c("Cost")
proj.length <- 5

#community.point <-left_join(community.point,city.shape,by=c("NAME"="city_name"))

# Census Data -------------------------------------------------------

census_data <- as_tibble(fread('data//census_data_by_place.csv'))
#census_data <- DBI::dbGetQuery(db.con, "execute census.census_data_for_member_profiles")
odbc::dbDisconnect(db.con)

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
