

# processing spatial data

library(dplyr)
library(sf)
library(stringr)
library(VicmapR)

#cfa_tfb <- st_read('shp/cfa_tfb_district.shp', stringsAsFactors = FALSE) 

tfb <- vicmap_query(layer = "datavic:VMADMIN_CFA_TFB_DISTRICT") %>% collect()

  

# CFA towers

twr <- st_read('shp/geomark_point.shp', stringsAsFactors = FALSE) %>%
  filter(STATE == 'VIC',
         FEATSUBTYP == 'fire lookout') %>%
  st_cast('POINT') %>%
  st_join(select(tfb, TFB_DISTRICT))


saveRDS(twr, 'towers.rds')
saveRDS(tfb, 'tfb.rds')


# create line using point and bearing

library(geosphere)

vis <- 40000 # visibility disance in metres
  
kg <- c(145.22466, -37.68855)
le <- c(145.52555, -37.56750)

l1 <- st_linestring(rbind(kg, destPoint(kg, 40, vis))) 

l2 <- st_linestring(rbind(le, destPoint(le, 40, vis))) 




plot(l1)
plot(l2, add=T)


twrs <- twr %>% filter(FEATURE_ID %in% towers)
twrs$bear = c(270,0,315)

dest = destPoint(st_coordinates(twrs), twrs$bear, 40000)


lines = st_coordinates(twrs) %>%
  cbind(dest) %>% 
  as.data.frame() %>% 
  split(1:nrow(.)) %>%
  map(., ~ st_linestring(rbind(c(.x$X,.x$Y),c(.x$lon,.x$lat)))) %>%
  st_as_sfc(crs = 4283)


mapview::mapview(lines)

tri <- lines %>%
  st_transform(3111) %>%
  st_intersection() %>% 
  st_as_sf(crs = 3111) %>%
  mutate(type = st_geometry_type(x)) %>%
  filter(type == 'POINT') 
  

# need to draw circle around points

# library(lwgeom)
# circ <- st_minimum_bounding_circle(st_union(tri))

# replace circ with boundary of tri centroid

cent <- st_convex_hull(st_union(tri)) %>% st_centroid() # convert to gda and reverse geocode

radius <- max(st_distance(tri, cent))

circ2 <- st_buffer(cent, radius) %>%
  st_transform(4283)

mapview(circ) + mapview(tri) + mapview(lines) + mapview(twrs) 
