library(sf)
library(tidyverse)
library(psrccensus)

# # disp_risk_path <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Displacement_Risk_Data/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
# 
# disp_risk_shp <- st_read(disp_risk_path)

# save new object as .rds file
# write_rds(disp_risk_shp, path = file.path('data', "disp_risk_shp.rds"))

# read in saved .rds
shp <- read_rds(file.path('data', "disp_risk_shp.rds"))


race_list <- acs_varsearch("race", year = 2019)
# B02001