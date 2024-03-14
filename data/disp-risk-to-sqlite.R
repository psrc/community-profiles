library(psrccensus)
library(tidycensus)
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

## creating helper columns
d <- df %>% 
  separate(variable,
           into = c("table", "var_num"),
           sep = "_") %>% 
  mutate(var_int = as.numeric(var_num))

## Aggregate categories ----


d_other <- d %>% 
  filter(var_int %in% 8:9) %>% 
  group_by(GEOID) %>% 
  summarise(estimate = sum(estimate),
            moe = tidycensus::moe_sum(moe, estimate)) %>% 
  mutate(label = 'Other',
         var_int = 8)

d_poc <- d %>% 
  filter(var_int %in% c(4:9, 12)) %>% 
  group_by(GEOID) %>% 
  summarise(estimate = sum(estimate),
            moe = tidycensus::moe_sum(moe, estimate)) %>% 
  mutate(label = 'POC',
         var_int = 9)
  
d2 <- d %>% 
  filter(var_int %in% c(1, 3:7, 12)) %>%
  bind_rows(d_other) %>% 
  bind_rows(d_poc) %>% 
  arrange(GEOID, var_int)

## Munge label ----


d3 <- d2 %>% 
  mutate(label2 = label) %>% 
  separate(label2,
           into = c("total", "ethnicity", "race", "detail_race"),
           sep = ":!!") %>%
  select(-detail_race) %>% 
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
                                          race_ethnicity == 'Hispanic or Latino' ~ 'Hispanic Or Latino',
                                          total == 'POC' ~ 'People of Color (POC)',
                                          total == 'Other' ~ 'Other'))

# Elmer Tract/Juris Splits ----


## tract/juris split values for 2010 tracts but split in accordance with OFM data for 2019
# http://aws-linux/mediawiki/index.php/Elmer_Connection_Configuration#Connection_via_R
# https://psrc.github.io/psrcelmer/
gsplit_sql <- "select * from general.get_geography_splits('tract10', 'Jurisdiction 2020', 2022, 2022, 2018)"
split_df <- get_query(gsplit_sql)

## use 2010-2020 census tract crosswalk
tract_xwalk <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Census_Tract_Relationships/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  st_drop_geometry()

## join 2022 data with tract crosswalk
d_w_xwalk <- left_join(d3, tract_xwalk, by = c('GEOID' = 'geoid20'))

## summarise 2022 data by 2010 Tracts
dt <- d_w_xwalk %>% 
  group_by(geoid10, race_ethnicity_label, concept, census_geography, acs_type, year, table, var_num, var_int) %>% 
  summarise(estimate = sum(estimate), 
            moe = tidycensus::moe_sum(moe, estimate)) %>% 
  arrange(geoid10, var_int) 

split_df <- split_df %>% 
  select(data_geog, planning_geog, percent_of_total_pop)

## join ACS data with Elmer splits
dt_join <- split_df %>% 
  full_join(dt, by = c('data_geog' = 'geoid10'), relationship = 'many-to-many') %>% 
  arrange(data_geog, var_int) %>% 
  mutate(split_totpop = estimate * percent_of_total_pop,
         split_totpop_moe = moe * percent_of_total_pop)

create_summary(df = dt_join, 
               grouping_vars = c('planning_geog', 'var_int', 'race_ethnicity_label'), 
               estimate_col = 'split_totpop') %>% 
  filter(planning_geog == 'Bellevue')

# QC ----

## check total pop calcs here
totpop_check <- dt_join %>% 
  group_by(planning_geog) %>% 
  filter(var_int == 1) %>% 
  summarise(split_totpop = sum(split_totpop))

## Demographic Profile 05 ----

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


## read in saved .rds
disp_risk <- read_rds(file.path('data', "disp_risk_shp.rds")) |>
  st_drop_geometry() |>
  select(countyfp10, county_name, geoid10, risk_level, risk_level_name)

## NAs in split_totpop/risk_level_name == 0 in estimate
dt_all <- dt_join %>% 
  filter(!is.na(planning_geog)) %>% 
  left_join(disp_risk, by = c("data_geog" = "geoid10")) %>% 
  mutate(planning_geog = str_replace_all(planning_geog, "Beaux Arts", "Beaux Arts Village")) %>% 
  mutate(planning_geog = str_replace_all(planning_geog, "Sea Tac", "SeaTac"))

create_summary(df = dt_all, 
               grouping_vars = c('planning_geog', 'var_int', 'race_ethnicity_label'), 
               estimate_col = 'split_totpop') %>% 
  filter(planning_geog == 'Bellevue')

summary_df <- dt_all %>% 
  group_by(planning_geog, race_ethnicity_label, risk_level_name) %>% 
  summarise(estimate = sum(split_totpop),
            moe = tidycensus::moe_sum(split_totpop_moe, split_totpop))

# Other aggregations ----

## add new risk level name: All
rl_all <- summary_df %>% 
  group_by(planning_geog, race_ethnicity_label) %>% 
  summarise(estimate = sum(estimate),
            moe = tidycensus::moe_sum(moe, estimate)) %>% 
  mutate(risk_level_name = 'All')

summary_df <- bind_rows(summary_df, rl_all) %>% 
  arrange(planning_geog, race_ethnicity_label, risk_level_name)

## add new juris: Region

region <- summary_df %>% 
  group_by(race_ethnicity_label, risk_level_name) %>% 
  summarise(estimate = sum(estimate),
            moe = tidycensus::moe_sum(moe, estimate)) %>% 
  mutate(planning_geog = 'Region')

summary_df <- bind_rows(summary_df, region)
  
# Formatting ----

re <- c('Total Population',
        'Hispanic Or Latino',
        'Non-Hispanic White',
        'Black or African American',
        'American Indian & Alaska Native',
        'Asian',
        'Native Hawaiian & Pacific Islander',
        'Other',
        'People of Color (POC)')

risk_levels <- c('lower', 'moderate', 'higher', 'All') 

summary_df_pivot <- summary_df %>% 
  mutate(race_ethnicity_label = factor(race_ethnicity_label, levels = re),
         risk_level_name = factor(risk_level_name, levels = risk_levels)) %>% 
  arrange(planning_geog, race_ethnicity_label, risk_level_name) %>% 
  pivot_wider(id_cols = c('planning_geog', 'race_ethnicity_label'),
              names_from = 'risk_level_name',
              values_from = c('estimate', 'moe'),
              names_expand = TRUE)

## create %

newcols <- paste0(paste0('estimate_', c('lower', 'moderate', 'higher')), "_share")

summary_df_pivot_all <- summary_df_pivot %>% 
  mutate(moe_lower_share = moe_prop(num = estimate_lower, 
                                    denom = estimate_All, 
                                    moe_num = moe_lower, 
                                    moe_denom = moe_All),
         moe_moderate_share = moe_prop(num = estimate_moderate, 
                                       denom = estimate_All, 
                                       moe_num = moe_moderate, 
                                       moe_denom = moe_All),
         moe_higher_share = moe_prop(num = estimate_higher, 
                                     denom = estimate_All, 
                                     moe_num = moe_higher, 
                                     moe_denom = moe_All)
           ) %>%
  mutate(estimate_lower_share = estimate_lower / estimate_All,
         estimate_moderate_share = estimate_moderate / estimate_All,
         estimate_higher_share = estimate_higher / estimate_All)




# send to mysqlite db ----

sqlite_dbname <- file.path('data', paste0('disp_risk_',Sys.Date(), '.db'))
mydb <- dbConnect(RSQLite::SQLite(), sqlite_dbname)

dbWriteTable(mydb, 'acs5_2022_B03002_tract_juris_split_summary', summary_df_pivot_all)
# tblnames <- paste0("acs5_2022_B03002_tract_juris_split_summary_", c("estimates", "shares"))
# walk2(tblnames, list( summary_df_pivot, summary_df_pivot_shares), ~dbWriteTable(mydb, .x , .y, overwrite = TRUE))

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

# test ----

# con <- dbConnect(SQLite(), "data/disp_risk_2024-03-14.db")
# as.data.frame(dbListTables(con))
# 
# # # Get table
# # test <- dbReadTable(con, 'acs5_2022_B03002_tract_juris_split_summary_estimates')
# # test2 <- dbReadTable(con, 'acs5_2022_B03002_tract_juris_split_summary_shares')
# test3 <- dbReadTable(con, 'acs5_2022_B03002_tract_juris_split_summary')
# 
# # # data is fetched; disconnect
# dbDisconnect(con)


