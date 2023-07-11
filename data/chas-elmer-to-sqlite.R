# Exports CHAS views from Elmer to a stand-alone SQLite database.

library(tidyverse)
library(odbc)
library(DBI)
library(RSQLite)

elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "AWS-PROD-SQL\\Sockeye",
                              database = "Elmer",
                              trusted_connection = "yes"
) 

sqlite_dbname <- file.path('data', paste0('chas_',Sys.Date(), '.db'))
mydb <- dbConnect(RSQLite::SQLite(), sqlite_dbname)

chas_table_codes <- c('T8', 'T15C', 'T14B', 'T9')
geog <- c('place', 'county', 'tract')

# export to SQLite db ----

for(g in geog) {
  print(paste("Starting ", g))
  query <-  paste0('execute chas.get_data_by_', g)
  exp <- map(chas_table_codes, ~paste0(query,'"', .x ,'"' ,', 2019'))
  dfs <- map(exp, ~dbGetQuery(elmer_connection, SQL(.x)))
  table_names <- paste0('chas.', g, "_", chas_table_codes)
  walk2(table_names, dfs, ~dbWriteTable(mydb, .x , .y, overwrite = TRUE))
  print(paste("... finished ", g))
}

# add index ----

tables <- dbListTables(mydb)
names <- unlist(map(tables, ~str_extract(.x, "(?<=\\.).*")))

for (n in names) {
  idx_name <- paste0('idx_', n, '_chas_year')
  fulltable <- paste0('[chas.', n, ']')
  sql <- paste0('CREATE INDEX ', idx_name, ' ON ', fulltable, '(chas_year)')
  print(sql)
  dbExecute(mydb, sql)
}

dbDisconnect(mydb)

# test ----

# con <- dbConnect(SQLite(), "data/chas_2023-07-11.db")
# as.data.frame(dbListTables(con))
# 
# # Get table
# test <- dbReadTable(con, 'chas.county_t14B')
# 
# # data is fetched; disconnect
# dbDisconnect(con)
