library(odbc)
library(DBI)

# connect to Elmer
db.connect <- function(adatabase) {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\Sockeye",
                                database = adatabase,
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function(adatabase, type = c('table', 'query'), string) {
  elmer_connection <- db.connect(adatabase)
  if(type == 'table') dtelm <- dbReadTable(elmer_connection, SQL(atable))
  if(type == 'query') dtelm <- dbGetQuery(elmer_connection, SQL(string))
  dbDisconnect(elmer_connection)
  setDT(dtelm)
}

gather_tables <- function(juris = c('place', 'tract', 'county'), chas_table_codes) {
  # gather CHAS tables of interest and store in a named list
  
  if(juris == 'place') {
    query <-  'execute chas.get_data_by_place'
  } else if (juris == 'tract') {
    query <-  'execute chas.get_data_by_tract'
  } else if (juris == 'county') {
    query <-  'execute chas.get_data_by_county'
  }
  
  exp <- map(chas_table_codes, ~paste0(query,'"', .x ,'"' ,', 2019'))
  
  dfs <- map(exp, ~read.dt('Elmer', 'query', .x))
  dfs <- map(dfs, ~.x[, sort := as.numeric(str_extract(variable_name, "\\d*$"))][order(sort)])
  names(dfs) <- chas_table_codes
  return(dfs)
}
