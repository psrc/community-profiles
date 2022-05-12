library(tidycensus)
library(tidyverse)
library(odbc)
library(DBI)
library(data.table)

Sys.getenv("CENSUS_API_KEY")

yrs <- c(2013,2014,2015,2018,2019,2020)
summary.tables.for.profiles <- c('DP02','DP03','DP04','DP05')
detailed.tables.for.profiles <- c('B08303')
counties <- c("King","Kitsap","Pierce","Snohomish")

variable.labels <- as_tibble(fread(paste0("data/data-profile-variables.csv")))

# Jurisdictions ----------------------------------------------------
server.name <- "AWS-PROD-SQL\\SOCKEYE"
database.name <- "Elmer"
jurisdiction.tbl <- "Political.jurisdiction_dims"

db.con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server.name,
                    database = database.name,
                    UID = Sys.getenv("userid"),
                    PWD = Sys.getenv("pwd")
)

places <- as_tibble(dbReadTable(db.con,SQL(jurisdiction.tbl))) %>%
  mutate(juris_name = gsub("Seatac","SeaTac",juris_name)) %>%
  mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",juris_name)) %>%
  mutate(juris_name = gsub("Uninc. King", "King County", juris_name)) %>%
  mutate(juris_name = gsub("Uninc. Kitsap", "Kitsap County", juris_name)) %>%
  mutate(juris_name = gsub("Uninc. Pierce", "Pierce County", juris_name)) %>%
  mutate(juris_name = gsub("Uninc. Snohomish", "Snohomish County", juris_name)) %>%
  select(juris_name) %>%
  distinct() %>%
  pull()

jurisdictions <- as_tibble(dbReadTable(db.con,SQL(jurisdiction.tbl))) %>%
  mutate(juris_name = gsub("Seatac","SeaTac",juris_name)) %>%
  mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",juris_name)) %>%
  select(juris_name, regional_geography, airport_affected) %>%
  distinct() %>%
  mutate(regional_geography=gsub("HCT","High Capacity Transit Community",regional_geography)) %>%
  mutate(regional_geography=gsub("Metro","Metropolitan Cities",regional_geography)) %>%
  mutate(regional_geography=gsub("Core","Core Cities",regional_geography)) %>%
  mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",regional_geography)) %>%
  select(-airport_affected)%>%
  mutate(juris_name = gsub("Uninc. King", "King County", juris_name)) %>%
  mutate(juris_name = gsub("Uninc. Kitsap", "Kitsap County", juris_name)) %>%
  mutate(juris_name = gsub("Uninc. Pierce", "Pierce County", juris_name)) %>%
  mutate(juris_name = gsub("Uninc. Snohomish", "Snohomish County", juris_name))

odbc::dbDisconnect(db.con)

airport.community <- c("Auburn", "Burien", "Covington", "Des Moines", "Federal Way", "Kenmore", "Kent", "Lake Forest Park",
                       "Normandy Park", "Renton", "SeaTac", "Seattle", "Skykomish", "Tukwila", "Bremerton", "Eatonville",
                       "Gig Harbor", "Lakewood", "Steilacoom", "Tacoma", "University Place", "Arlington", "Darrington",
                       "Everett", "Marysville", "Monroe","Mukilteo", "Snohomish", "Sultan")

airport.community <- enframe(airport.community) %>%
  mutate(airport_affected = "Yes") %>%
  select(-name)

jurisdictions <- left_join(jurisdictions, airport.community, by=c("juris_name"="value")) %>%
  mutate(across(airport_affected, ~replace_na(.x, "No")))  %>%
  mutate(regional_geography = case_when(
    juris_name %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County") ~ "Not Applicable",
    !(juris_name %in% c("King County", "Kitsap County", "Pierce County", "Snohomish County")) ~ regional_geography))

rm(airport.community)

# Census Data Profiles Data ----------------------------------------------------
census.data <- NULL
for (y in yrs) {
  l <- variable.labels %>% filter(Year == y) %>% select(-Year)
  c <- l %>% select(Variable) %>% pull() %>% unique()
  tv <- l %>% select(Total) %>% pull() %>% unique()
  
  for (t in summary.tables.for.profiles) {
    
    # County
    d <- get_acs(state='Washington', geography='county', county=counties, year=y, survey="acs5", table=t)
    d <- d %>% mutate(NAME = gsub(", Washington", "", NAME))
    v <- d %>% filter(!(endsWith(variable,"P")))
    p <- d %>% filter(endsWith(variable,"P")) %>% rename(estimate_percent=estimate, moe_percent=moe) %>% mutate(variable = str_sub(variable,1,nchar(variable)-1))
    e <- left_join(v, p, by=c("GEOID", "NAME", "variable")) 
    
    # Create a Region Total
    r <- e %>%
      select(-GEOID,-NAME) %>%
      group_by(variable) %>%
      summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE, .names = "{col}")) %>%
      mutate(GEOID="53033035053061", NAME="Region")
    
    e <- bind_rows(e, r)
    
    e  <- e %>% 
      mutate(census_year=y, place_type="co", acs_type="acs5") %>% 
      rename(geoid=GEOID, geog_name=NAME, variable_name=variable, margin_of_error=moe)
    
    # Filter Down to only Variables used in the Data Profiles and add Labels
    e <- e %>% filter(variable_name %in% c)
    e <- left_join(e, l, by=c("variable_name"="Variable"))
    
    # Add Totals for use in Share Calculations
    totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
    e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
    e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract, -Tract_Agg)
    
    ifelse(is.null(census.data), census.data <- e, census.data <- bind_rows(census.data,e))
    rm(d, v, p, e, totals, r)
    
    # Place
    d <- get_acs(state='Washington', geography='place', year=y, survey="acs5", table=t)
    d <- d %>% 
      mutate(NAME = gsub("Silverdale CDP, Washington", "Silverdale", NAME)) %>%
      filter(!(str_detect(NAME, "CDP"))) %>% 
      mutate(NAME = gsub(" city, Washington", "", NAME), NAME = gsub(" town, Washington", "", NAME)) %>%
      filter(NAME %in% places)
    v <- d %>% filter(!(endsWith(variable,"P")))
    p <- d %>% filter(endsWith(variable,"P")) %>% rename(estimate_percent=estimate, moe_percent=moe) %>% mutate(variable = str_sub(variable,1,nchar(variable)-1))
    e <- left_join(v, p, by=c("GEOID", "NAME", "variable")) 
    e <- e %>% 
      mutate(census_year=y, place_type="pl", acs_type="acs5") %>% 
      rename(geoid=GEOID, geog_name=NAME, variable_name=variable, margin_of_error=moe)
    
    # Filter Down to only Variables used in the Data Profiles and add Labels
    e <- e %>% filter(variable_name %in% c)
    e <- left_join(e, l, by=c("variable_name"="Variable"))
    
    # Add Totals for use in Share Calculations
    totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
    e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
    e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract, -Tract_Agg)
    
    ifelse(is.null(census.data), census.data <- e, census.data <- bind_rows(census.data,e))
    rm(d, v, p, e, totals)
    
    # Tracts
    d <- get_acs(state='Washington', geography='tract', year=y, survey="acs5", table=t) %>% 
      separate(NAME, into=c("name","county", "state"),sep=",") %>%
      mutate(county = trimws(county, "l"), state = trimws(state, "l")) %>%
      filter(county %in% c("King County","Kitsap County","Pierce County","Snohomish County"))
    v <- d %>% filter(!(endsWith(variable,"P")))
    p <- d %>% filter(endsWith(variable,"P")) %>% rename(estimate_percent=estimate, moe_percent=moe) %>% mutate(variable = str_sub(variable,1,nchar(variable)-1))
    e <- left_join(v, p, by=c("GEOID", "name", "county","state","variable")) 
    e <- e %>% 
      mutate(census_year=y, place_type="tr", acs_type="acs5") %>% 
      select(-county, -state) %>%
      rename(geoid=GEOID, geog_name=name, variable_name=variable, margin_of_error=moe)
    
    # Filter Down to only Variables used in the Data Profiles and add Labels
    e <- e %>% filter(variable_name %in% c)
    e <- left_join(e, l, by=c("variable_name"="Variable"))
    e <- e %>% filter(Tract %in% c("Yes"))
    
    # Add Totals for use in Share Calculations
    totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
    e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
    e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract)
    
    tract.data <- e %>% filter(Tract_Agg != "") %>%
      group_by(geoid,geog_name,census_year,place_type,acs_type,Category,Tract_Agg) %>%
      summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE, .names = "{col}"))
    
    tract.data <- as_tibble(tract.data) %>% mutate(variable_name = "Tract Variables", Label=Tract_Agg) %>% select(-Tract_Agg)
    
    census.data <- bind_rows(census.data,tract.data)
    rm(d, v, p, e, totals, tract.data)
    
  } # end of table loop by current year
  rm(l, c, tv)
  
} # end of year loop

# Census Data Travel Time Data ----------------------------------------------------
for (y in yrs) {
  l <- variable.labels %>% filter(Year == y) %>% select(-Year)
  c <- l %>% select(Variable) %>% pull() %>% unique()
  tv <- l %>% select(Total) %>% pull() %>% unique()
  
  # County
  d <- get_acs(state='Washington', geography='county', county=counties, year=y, survey="acs5", table='B08303')
  d <- d %>%
    mutate(NAME = gsub(", Washington", "", NAME))
    
  v <- d %>% filter(variable %in% c("B08303_001")) %>% select(-variable) %>% rename(total=estimate, total_moe=moe)
  d <- left_join(d,v,by=c("GEOID","NAME"))
  d <- d %>%
    mutate(estimate_percent = estimate/total, moe_percent = moe/total) %>%
    select(-total, -total_moe)
  
  # Create a Region Total
  r <- d %>%
    select(-GEOID,-NAME) %>%
    group_by(variable) %>%
    summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE, .names = "{col}")) %>%
    mutate(GEOID="53033035053061", NAME="Region")
  
  d <- bind_rows(d, r)
    
  e  <- d %>% 
    mutate(census_year=y, place_type="co", acs_type="acs5") %>% 
    rename(geoid=GEOID, geog_name=NAME, variable_name=variable, margin_of_error=moe)
    
  # Filter Down to only Variables used in the Data Profiles and add Labels
  e <- e %>% filter(variable_name %in% c)
  e <- left_join(e, l, by=c("variable_name"="Variable"))
    
  # Add Totals for use in Share Calculations
  totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
  e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
  e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract, -Tract_Agg)
    
  ifelse(is.null(census.data), census.data <- e, census.data <- bind_rows(census.data,e))
  rm(d, e, v, totals, r)
    
  # Place
  d <- get_acs(state='Washington', geography='place', year=y, survey="acs5", table='B08303')
  d <- d %>% 
    mutate(NAME = gsub("Silverdale CDP, Washington", "Silverdale", NAME)) %>%
    filter(!(str_detect(NAME, "CDP"))) %>%
    mutate(NAME = gsub(" city, Washington", "", NAME), NAME = gsub(" town, Washington", "", NAME)) %>%
    filter(NAME %in% places)
    
  v <- d %>% filter(variable %in% c("B08303_001")) %>% select(-variable) %>% rename(total=estimate, total_moe=moe)
  d <- left_join(d,v,by=c("GEOID","NAME"))
  d <- d %>%
    mutate(estimate_percent = estimate/total, moe_percent = moe/total) %>%
    select(-total, -total_moe)
    
  e  <- d %>% 
    mutate(census_year=y, place_type="pl", acs_type="acs5") %>% 
    rename(geoid=GEOID, geog_name=NAME, variable_name=variable, margin_of_error=moe)
    
  # Filter Down to only Variables used in the Data Profiles and add Labels
  e <- e %>% filter(variable_name %in% c)
  e <- left_join(e, l, by=c("variable_name"="Variable"))
  
  # Add Totals for use in Share Calculations
  totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
  e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
  e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract, -Tract_Agg)
    
  census.data <- bind_rows(census.data,e)
  rm(d, e, v, totals, l, c, tv)
  
} # end of year loop

# Census Data Travel Departure Time Data ----------------------------------------------------
for (y in yrs) {
  l <- variable.labels %>% filter(Year == y) %>% select(-Year)
  c <- l %>% select(Variable) %>% pull() %>% unique()
  tv <- l %>% select(Total) %>% pull() %>% unique()
  
  # County
  d <- get_acs(state='Washington', geography='county', county=counties, year=y, survey="acs5", table='B08302')
  d <- d %>%
    mutate(NAME = gsub(", Washington", "", NAME))
  
  v <- d %>% filter(variable %in% c("B08302_001")) %>% select(-variable) %>% rename(total=estimate, total_moe=moe)
  d <- left_join(d,v,by=c("GEOID","NAME"))
  d <- d %>%
    mutate(estimate_percent = estimate/total, moe_percent = moe/total) %>%
    select(-total, -total_moe)
  
  # Create a Region Total
  r <- d %>%
    select(-GEOID,-NAME) %>%
    group_by(variable) %>%
    summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE, .names = "{col}")) %>%
    mutate(GEOID="53033035053061", NAME="Region")
  
  d <- bind_rows(d, r)
  
  e  <- d %>% 
    mutate(census_year=y, place_type="co", acs_type="acs5") %>% 
    rename(geoid=GEOID, geog_name=NAME, variable_name=variable, margin_of_error=moe)
  
  # Filter Down to only Variables used in the Data Profiles and add Labels
  e <- e %>% filter(variable_name %in% c)
  e <- left_join(e, l, by=c("variable_name"="Variable"))
  
  # Add Totals for use in Share Calculations
  totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
  e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
  e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract, -Tract_Agg)
  
  ifelse(is.null(census.data), census.data <- e, census.data <- bind_rows(census.data,e))
  rm(d, e, v, totals, r)
  
  # Place
  d <- get_acs(state='Washington', geography='place', year=y, survey="acs5", table='B08302')
  d <- d %>% 
    mutate(NAME = gsub("Silverdale CDP, Washington", "Silverdale", NAME)) %>%
    filter(!(str_detect(NAME, "CDP"))) %>%
    mutate(NAME = gsub(" city, Washington", "", NAME), NAME = gsub(" town, Washington", "", NAME)) %>%
    filter(NAME %in% places)
  
  v <- d %>% filter(variable %in% c("B08302_001")) %>% select(-variable) %>% rename(total=estimate, total_moe=moe)
  d <- left_join(d,v,by=c("GEOID","NAME"))
  d <- d %>%
    mutate(estimate_percent = estimate/total, moe_percent = moe/total) %>%
    select(-total, -total_moe)
  
  e  <- d %>% 
    mutate(census_year=y, place_type="pl", acs_type="acs5") %>% 
    rename(geoid=GEOID, geog_name=NAME, variable_name=variable, margin_of_error=moe)
  
  # Filter Down to only Variables used in the Data Profiles and add Labels
  e <- e %>% filter(variable_name %in% c)
  e <- left_join(e, l, by=c("variable_name"="Variable"))
  
  # Add Totals for use in Share Calculations
  totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
  e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
  e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract, -Tract_Agg)
  
  census.data <- bind_rows(census.data,e)
  rm(d, e, v, totals)
  
  # Tracts
  d <- get_acs(state='Washington', geography='tract', year=y, survey="acs5", table='B08302') %>% 
    separate(NAME, into=c("name","county", "state"),sep=",") %>%
    mutate(county = trimws(county, "l"), state = trimws(state, "l")) %>%
    filter(county %in% c("King County","Kitsap County","Pierce County","Snohomish County"))
  
  e  <- d %>% 
    mutate(census_year=y, place_type="tr", acs_type="acs5") %>% 
    rename(geoid=GEOID, geog_name=name, variable_name=variable, margin_of_error=moe)
  
  # Filter Down to only Variables used in the Data Profiles and add Labels
  e <- e %>% filter(variable_name %in% c)
  e <- left_join(e, l, by=c("variable_name"="Variable"))
  e <- e %>% filter(Tract %in% c("Yes"))
  
  # Add Totals for use in Share Calculations
  totals <- e %>% filter(variable_name %in% tv) %>% select(geoid, variable_name, estimate) %>% rename(total_for_share=estimate)
  e <- left_join(e, totals, by=c("Total"="variable_name", "geoid"))
  e <- e %>% mutate(share = estimate/total_for_share) %>% select(-Total, -Tract)
  
  tract.data <- e %>% filter(Tract_Agg != "") %>%
    group_by(geoid,geog_name,census_year,place_type,acs_type,Category,Tract_Agg) %>%
    summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE, .names = "{col}"))
  
  tract.data <- as_tibble(tract.data) %>% mutate(variable_name = "Tract Variables", Label=Tract_Agg) %>% select(-Tract_Agg)
  
  census.data <- bind_rows(census.data,tract.data)
  rm(d, e, totals, tract.data, l, c, tv)
  
} # end of year loop

census.data <- census.data %>%
  mutate(moe_percent = replace(moe_percent, is.infinite(moe_percent), NA)) %>%
  mutate(estimate_percent = replace(estimate_percent, is.nan(estimate_percent), NA))

fwrite(census.data, 'data//census_data_by_place.csv')
fwrite(jurisdictions, 'data//jurisdictions.csv')
