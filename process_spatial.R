

# processing spatial data

library(dplyr)
library(sf)
library(stringr)

cfa_tfb <- st_read('shp/cfa_tfb_district.shp', stringsAsFactors = FALSE) 



  

# CFA towers

twr <- st_read('shp/geomark_point.shp', stringsAsFactors = FALSE) %>%
  filter(STATE == 'VIC',
         FEATSUBTYP == 'fire lookout',
         !grepl('SATELLITE|&|infrastructure', NAME, ignore.case = TRUE)) %>%
  select(town_name = NAME_LABEL) %>%
  mutate(town_name = str_trim(gsub('Fire Station|District', '', town_name), 'both'),
         town_name = gsub('\\.', '', town_name),
         town_val = tolower(gsub(' | - ', '-', town_name))) %>%
  st_join(cfa_tfb) %>%
  select(town_name,
         town_val,
         cfa_tfb = TFB_DIST) %>%
  mutate(cfa_tfb = tools::toTitleCase(tolower(cfa_tfb))) %>%
  st_cast('POINT')



# create line using point and bearing

library(geosphere)

vis <- 40000 # visibility disance in metres
  
kg <- c(145.22466, -37.68855)
le <- c(145.52555, -37.56750)

l1 <- st_linestring(rbind(kg, destPoint(kg, 40, vis))) 

l2 <- st_linestring(rbind(le, destPoint(le, 40, vis))) 

saveRDS(twr, 'towers.rds')

