library(psrccensus)
library(tidyverse)
library(psrcelmer)
library(sf)
library(odbc)
library(DBI)
library(RSQLite)

# Functions ----

create_summary <- function(df, grouping_vars, estimate_col) {
  # summarise
  
  df %>% 
    group_by(across(all_of(grouping_vars))) %>% 
    summarise(estimate = sum(.data[[estimate_col]]))
}


# B03002 Hispanic/Latino Origin by Race (Universe: Total Population)
# is this data already in elmer?
df <- get_acs_recs("tract", table.names = "B03002", years = 2022, acs.type = 'acs5') 

# creating helper columns
d <- df %>% 
  separate(variable,
           into = c("table", "var_num"),
           sep = "_") %>% 
  mutate(var_int = as.numeric(var_num))

# read-in Elmer Tract/Juris Split
# http://aws-linux/mediawiki/index.php/Elmer_Connection_Configuration#Connection_via_R
# https://psrc.github.io/psrcelmer/

# tract/juris split values for 2010 tracts but split in accordance with OFM data for 2019
gsplit_sql <- "select * from general.get_geography_splits('tract10', 'Jurisdiction 2020', 2019, 2020, 2018)"
split_df <- get_query(gsplit_sql)

# use 2010-2020 census tract crosswalk
tract_xwalk <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Census_Tract_Relationships/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  st_drop_geometry()

# join 2022 data with tract crosswalk
d_w_xwalk <- left_join(d, tract_xwalk, by = c('GEOID' = 'geoid20'))

# summarise 2022 data by 2010 Tracts
dt <- d_w_xwalk %>% 
  group_by(geoid10, label, concept, census_geography, acs_type, year, table, var_num, var_int) %>% 
  summarise(estimate = sum(estimate), 
            moe = tidycensus::moe_sum(moe, estimate)) %>% 
  arrange(geoid10, var_int) 

# munge
dt2 <- dt %>% 
  mutate(label2 = label,
         var_int = as.numeric(var_num)) %>% 
  separate(label2,
           into = c("total", "ethnicity", "race", "detail_race"),
           sep = ":!!") %>%
  filter(var_int %in% c(1, 3:9, 12)) %>%
  select(-detail_race)

split_df <- split_df %>% 
  select(data_geog, planning_geog, percent_of_total_pop)

# join ACS data with Elmer splits
dt_join <- split_df %>% 
  full_join(dt2, by = c('data_geog' = 'geoid10'), relationship = 'many-to-many') %>% 
  arrange(data_geog, var_int) %>% 
  mutate(split_totpop = estimate * percent_of_total_pop) %>% 
  mutate(race_ethnicity = race) %>%
  mutate(race_ethnicity = case_when(var_int == 1 ~ 'Total',
                                    var_int == 12 ~ 'Hispanic or Latino',
                                    .default = race_ethnicity)) %>%
  mutate(race_ethnicity = str_replace_all(race_ethnicity, ":", "")) %>%
  mutate(race_ethnicity_label = case_when(race_ethnicity == 'Total' ~ 'Total Population',
                                          race_ethnicity == 'White alone' ~ 'Non-Hispanic White',
                                          race_ethnicity == 'Black or African American alone' ~ 'Black or African American',
                                          race_ethnicity == 'American Indian and Alaska Native alone' ~ 'American Indian & Alaska Native',
                                          race_ethnicity == 'Asian alone' ~ 'Asian',
                                          race_ethnicity == 'Native Hawaiian and Other Pacific Islander alone' ~ 'Native Hawaiian & Pacific Islander',
                                          race_ethnicity == 'Some other race alone' ~ 'Some Other Race',
                                          race_ethnicity == 'Two or more races' ~ 'Two or More Races',
                                          race_ethnicity == 'Hispanic or Latino' ~ 'Hispanic Or Latino')) %>% 
  select(-label, -race_ethnicity, -total, -ethnicity, -race)

create_summary(df = dt_join, 
               grouping_vars = c('planning_geog', 'var_int', 'race_ethnicity_label'), 
               estimate_col = 'split_totpop')


# QC Bellevue ----
# Bellevue checks good

# bellevue_tracts <- split_df %>% 
#   filter(planning_geog == 'Bellevue') %>% 
#   distinct(data_geog)
# 
# bellevue_tracts2 <- split_df %>% 
#   filter(data_geog %in% bellevue_tracts$data_geog)
# 
# bellevue_dt2 <- dt2 %>% 
#   ungroup() %>%
#   select(var_int, geoid10, label,  estimate, moe) %>% 
#   filter(geoid10 %in% bellevue_tracts$data_geog)
# 
# bellevue_df <- bellevue_tracts2 %>% 
#   left_join(bellevue_dt2, by = c('data_geog' = 'geoid10')) %>% 
#   arrange(data_geog, var_int) %>% 
#   mutate(split_totpop = estimate * percent_of_total_pop)
#  
# bellevue_df_sum <- bellevue_df %>% 
#   group_by(planning_geog, var_int, label) %>% 
#   summarise(split_totpop = sum(split_totpop))
# 
# bellevue_df_sum %>% 
#   ungroup() %>% 
#   filter(planning_geog == "Bellevue" & var_int != 1) %>% 
#   summarise(split_totpop = sum(split_totpop))

# QC ----

# check total pop calcs here
totpop_check <- dt_join %>% 
  group_by(planning_geog) %>% 
  filter(var_int == 1) %>% 
  summarise(split_totpop = sum(split_totpop))

# Demographic Profile 05 ----

dp05 <- get_acs_recs(geography = "place",
                     table.names = "DP05",
                     years = 2022,
                     acs.type = "acs5")

dp05_totpop <- dp05 %>% 
  filter(variable == "DP05_0001" & (census_geography != 'CDP' | is.na(census_geography))) %>% 
  mutate(place = str_replace_all(name, "town", "")) %>% 
  mutate(place = str_replace_all(place, "\\(Pierce County\\)", "")) %>% 
  mutate(place = str_replace_all(place, "\\(King County\\)", "")) %>% 
  mutate(place = str_replace_all(place, "Village", "")) %>% 
  mutate(place = str_replace_all(place, "SeaTac", "Sea Tac")) %>% 
  mutate(place = str_trim(place)) %>% 
  select(place, dp05_totpop = estimate, dp05_moe = moe)

qc <- totpop_check %>% 
  left_join(dp05_totpop, by = c('planning_geog' = 'place')) %>% 
  mutate(diff = split_totpop - dp05_totpop)

# openxlsx::write.xlsx(qc, "data/qc_splittotpop_dp05_totpop_only.xlsx")

# Displacement Risk Levels ----

# read in saved .rds
disp_risk <- read_rds(file.path('data', "disp_risk_shp.rds")) |>
  st_drop_geometry() |>
  select(countyfp10, county_name, geoid10, risk_level, risk_level_name)

# NAs in split_totpop/risk_level_name == 0 in estimate
dt_all <- dt_join %>% 
  filter(!is.na(planning_geog)) %>% 
  left_join(disp_risk, by = c("data_geog" = "geoid10")) 

create_summary(df = dt_all, 
               grouping_vars = c('planning_geog', 'var_int', 'race_ethnicity_label'), 
               estimate_col = 'split_totpop')

summary_df <- dt_all %>% 
  group_by(planning_geog, race_ethnicity_label, risk_level_name) %>% 
  summarise(estimate = sum(split_totpop))

## add new risk level name: All
rl_all <- summary_df %>% 
  group_by(planning_geog, race_ethnicity_label) %>% 
  summarise(estimate = sum(estimate)) %>% 
  mutate(risk_level_name = 'All')

summary_df <- bind_rows(summary_df, rl_all) %>% 
  arrange(planning_geog, race_ethnicity_label, risk_level_name)
  
# Formatting ----

re <- c('Total Population',
        'Hispanic Or Latino',
        'Non-Hispanic White',
        'Black or African American',
        'American Indian & Alaska Native',
        'Asian',
        'Native Hawaiian & Pacific Islander',
        'Some Other Race',
        'Two or More Races')

risk_levels <- c('lower', 'moderate', 'higher', 'All') 

summary_df_pivot <- summary_df %>% 
  mutate(race_ethnicity_label = factor(race_ethnicity_label, levels = re),
         risk_level_name = factor(risk_level_name, levels = risk_levels)) %>% 
  arrange(planning_geog, race_ethnicity_label, risk_level_name) %>% 
  pivot_wider(id_cols = c('planning_geog', 'race_ethnicity_label'),
              names_from = 'risk_level_name',
              values_from = 'estimate',
              names_expand = TRUE)

## create %
summary_df_pivot_shares <- summary_df_pivot %>% 
  mutate(across(lower:higher, ~ . / All))


# send to mysqlite db ----

sqlite_dbname <- file.path('data', paste0('disp_risk_',Sys.Date(), '.db'))
mydb <- dbConnect(RSQLite::SQLite(), sqlite_dbname)

tblnames <- paste0("acs5_2022_B03002_tract_juris_split_summary_", c("estimates", "shares"))
walk2(tblnames, list( summary_df_pivot, summary_df_pivot_shares), ~dbWriteTable(mydb, .x , .y, overwrite = TRUE))

# add index? ----

# tables <- dbListTables(mydb)
# names <- unlist(map(tables, ~str_extract(.x, "(?<=\\.).*")))
# 
# for (n in names) {
#   idx_name <- paste0('idx_', n, '_chas_year')
#   fulltable <- paste0('[chas.', n, ']')
#   sql <- paste0('CREATE INDEX ', idx_name, ' ON ', fulltable, '(chas_year)')
#   print(sql)
#   dbExecute(mydb, sql)
# }

dbDisconnect(mydb)

# # test ----
# 
# con <- dbConnect(SQLite(), "data/disp_risk_2024-03-06.db")
# as.data.frame(dbListTables(con))
# 
# # # Get table
# test <- dbReadTable(con, 'acs5_2022_B03002_tract_juris_split_summary_estimates')
# test2 <- dbReadTable(con, 'acs5_2022_B03002_tract_juris_split_summary_shares')
# 
# # # data is fetched; disconnect
# dbDisconnect(con)

