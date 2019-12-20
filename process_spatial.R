

# processing spatial data

library(dplyr)
library(sf)
library(stringr)

cfa_tfb <- st_read('shp/cfa_tfb_district.shp', stringsAsFactors = FALSE) 



  

# CFA towers

twr <- st_read('shp/geomark_point.shp', stringsAsFactors = FALSE) %>%
  filter(STATE == 'VIC',
         FEATSUBTYP == 'fire lookout') %>%
  st_cast('POINT')


saveRDS(twr, 'towers.rds')



# create line using point and bearing

library(geosphere)

vis <- 40000 # visibility disance in metres
  
kg <- c(145.22466, -37.68855)
le <- c(145.52555, -37.56750)

l1 <- st_linestring(rbind(kg, destPoint(kg, 40, vis))) 

l2 <- st_linestring(rbind(le, destPoint(le, 40, vis))) 



