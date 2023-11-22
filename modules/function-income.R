# Function to assemble Income table

source('modules/function-query-sqlite-chas.R')

create_income_table <- function(juris = c('place', 'county', 'region')) {
  
  chas_tables <- 'T1'
  ifelse(juris == 'place', j <- 'place', j <- 'county')
  dfs <- gather_tables(j, chas_tables)
  
  # Assemble Table ----
  desc <- c('Extremely Low-Income (≤30% AMI)',
            'Very Low-Income (30-50%)',
            'Low-Income (50-80%)',
            'Moderate Income (80-100%)',
            'Greater than 100% AMI',
            'Up to 80% AMI',
            'All')
  
  cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe',  'tenure',
            'household_income', 'race_ethnicity', 'income_grp', 'race_ethnicity_grp')

  dfs$T1[, income_grp := fcase(household_income == 'All', 'All',
                               household_income == 'less than or equal to 30% of HAMFI', 'Extremely Low-Income (≤30% AMI)',
                               household_income == 'greater than 30% but less than or equal to 50% of HAMFI', 'Very Low-Income (30-50%)',
                               household_income == 'greater than 50% but less than or equal to 80% of HAMFI', 'Low-Income (50-80%)',
                               household_income == 'greater than 80% but less than or equal to 100% of HAMFI', 'Moderate Income (80-100%)',
                               household_income == 'greater than 100% of HAMFI', 'Greater than 100% AMI')]
  
  dfs$T1[, race_ethnicity_grp := fcase(grepl("^American Indian ", race_ethnicity), "American Indian and Alaska Native",
                                       grepl("^Asian ", race_ethnicity), "Asian",
                                       grepl("^Black ", race_ethnicity), "Black or African American",
                                       grepl("^Hispanic, any race", race_ethnicity), "Hispanic or Latino (of any race)",
                                       grepl("^Pacific ", race_ethnicity), "Pacific Islander",
                                       grepl("^White ", race_ethnicity), "White",
                                       grepl("^All", race_ethnicity), "All")]
  
  # exclude high level totals
  df <- dfs$T1[!(sort %in% c(1, 2, 75)),]
  
  # factors & levels
  df <- df[, ..cols]
  
  race_levels <- c(str_subset(unique(df$race_ethnicity_grp), "^American.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Asian.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Black.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Hispanic.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Pacific.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Other.*"),
                   'People of Color (POC)',
                   str_subset(unique(df$race_ethnicity_grp), "^White.*"),
                   'Not Reported',
                   'All')
  
  df[, race_ethnicity_grp := factor(race_ethnicity_grp, levels = race_levels)]
  
  if(juris == 'region') {
    # aggregate counties to region
    
    df <- df[, .(estimate = sum(estimate)), by = c('variable_name', 'sort', 'chas_year', 'income_grp', 'race_ethnicity_grp', 'tenure')
    ][, geography_name := 'Region'] 
  }
  
  df_sum <- df[, .(estimate = sum(estimate), 
                   moe = moe_sum(moe, estimate)),
               by = c('chas_year', 'geography_name', 'tenure', 'income_grp', 'race_ethnicity_grp')]
  
  # sum each race/ethnicity/POC
  tot_resp_race <- df_sum[income_grp != 'All' & race_ethnicity_grp != 'All', 
                          .(estimate = sum(estimate), moe = moe_sum(moe, estimate), income_grp = 'All'), 
                          by = c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp')]
  
  # sum POC (totals & by income group)
  poc <- df_sum[!race_ethnicity_grp %in% c('All', str_subset(race_ethnicity_grp, "^[W].*")), 
                .(estimate = sum(estimate), moe = moe_sum(moe, estimate), race_ethnicity_grp = 'People of Color (POC)'),
                by = c('chas_year', 'geography_name', 'tenure', 'income_grp')]

  tot_poc <- poc[, .(estimate = sum(estimate), 
                     moe = moe_sum(moe, estimate), 
                     income_grp = 'All'), 
                 by = c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp') ]
  
  # combine
  df_all <- rbindlist(list(df_sum, tot_resp_race, poc, tot_poc), use.names=TRUE, fill = TRUE)
  
  # incorporate race/ethnicity, poc, and all income totals for denominator column
  denom <- rbindlist(list(tot_resp_race, tot_poc), use.names=TRUE)
  setnames(denom, c('estimate', 'moe'), c('denom', 'moe_denom'))
  denom[, income_grp:= NULL]
  
  all <- df_all[income_grp == 'All' & race_ethnicity_grp == 'All', ][, income_grp := NULL]
  setnames(all, c('estimate', 'moe'), c('denom', 'moe_denom'))
  denom <- rbindlist(list(denom, all))
  
  df_join <- merge(df_all, denom, by =  c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp'), all.x = TRUE)
  
  # calculate Up to 80%
  up80 <- df_join[income_grp %in% desc[1:3], 
                  .(estimate = sum(estimate), 
                    moe = moe_sum(moe, estimate),
                    income_grp = 'Up to 80% AMI'), 
                  by = c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp')]
  
  up80_join <- merge(up80, denom, by = c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp'), all.x = TRUE)
  
  df_join <- rbindlist(list(df_join, up80_join), use.names=TRUE)
  
  df_join <- df_join[, income_grp := factor(income_grp, levels = desc)]
  
  # calculate Not Reported
  #### FIGURE OUT MOE ----
  re_sum <- df_join[!(race_ethnicity_grp %in% c('People of Color (POC)', 'All')), 
                    .(sum = sum(estimate), 
                      moe_sum = moe_sum(moe, estimate),
                      race_ethnicity_grp = 'Not Reported'), 
                    by = c('chas_year', 'geography_name', 'tenure', 'income_grp')]
  
  re_total <- df_join[race_ethnicity_grp == 'All', .(geography_name, tenure, income_grp, Total = estimate, moe_total = moe)]
  re_sum_join <- merge(re_sum, re_total, by = c('geography_name', 'tenure', 'income_grp'), all.x=TRUE)
  re_sum_join[, estimate := Total - sum]
  #re_sum_join[, estimate := Total - sum][, `:=` (sum = NULL, Total = NULL)]
  
  browser()
  #### HOW TO CALC MOE DIFFERENCE?
  
  # test <- re_sum_join %>% 
  #   rowwise() %>% 
  #   mutate(estimate_moe = moe_sum(estimate = c(Total, sum),
  #                                 moe = c(moe_total, moe_sum)))
  
  
  
  
  # any negative differences equals over reporting. Change result to 0.
  re_sum_join[estimate < 0, estimate := 0]
  
  re_sum_all <- re_sum_join[income_grp == 'All', .(geography_name, tenure, denom = estimate)]
  re_sum_join <- merge(re_sum_join, re_sum_all, by = c('geography_name', 'tenure'), all.x=TRUE)
  
  df_join <- rbindlist(list(df_join, re_sum_join), use.names=TRUE)
  
  # create shares
  df_join[, share := estimate/denom]
  df_join[is.na(share), share := 0]

  # pivot wider
  df_est <- dcast.data.table(df_join, chas_year + geography_name + tenure + race_ethnicity_grp ~ income_grp, value.var = 'estimate')
  df_shr <- dcast.data.table(df_join, chas_year + geography_name + tenure + race_ethnicity_grp ~ income_grp, value.var = 'share')
  
  return(list(e = df_est, s = df_shr))
}

# x <- create_income_table(juris = 'region')
# y <- create_income_table(juris = 'county')
z <- create_income_table(juris = 'place')

create_dt_income <- function(table, container, source) {
  datatable(table,
            container = container,
            rownames = FALSE,
            options = list(dom = 'tipr',
                           columnDefs = list(list(className = 'dt-center', targets = 1:9))),
            caption = htmltools::tags$caption(
              style = 'caption-side: bottom; text-align: right;',
              htmltools::em(source)
            ))
}
