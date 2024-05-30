library(RSQLite)
library(odbc)
library(DBI)
library(tidyverse)
library(data.table)
library(tidycensus)

sqldb.connect.disprisk <- function() {
  # connect to SQLite db containing tract-juris split displacement risk level tables
  
  dbConnect(SQLite(), "data/disp_risk_2024-04-04.db")
}


read.dt.disprisk <- function(type = c('table', 'query'), string) {
  # read table
  
  con <- sqldb.connect.disprisk()
  if(type == 'table') dt <- dbReadTable(con, string)
  if(type == 'query') dt <- dbGetQuery(con, string)
  dbDisconnect(con)
  setDT(dt)
}


