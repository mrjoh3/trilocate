

# processing spatial data

library(dplyr)
library(sf)
library(VicmapR)


# map zoom area (OPTIONAL)
tfb <- vicmap_query(layer = "datavic:VMADMIN_CFA_TFB_DISTRICT") %>% collect() %>%
  st_simplify()


saveRDS(tfb, 'tfb.rds')



# towers (REQUIRED)
twr <- vicmap_query(layer = "datavic:VMFEAT_GEOMARK_POINT") %>% 
  filter(FEATURE_SUBTYPE == 'fire lookout') %>% collect() %>%
  st_cast('POINT') %>%
  select(id = FEATURE_ID,
         type = FEATURE_SUBTYPE,
         name = NAME_LABEL,
         state = STATE) %>%
  st_join(select(tfb, TFB_DISTRICT)) %>%
  mutate(TFB_DISTRICT = ifelse(is.na(TFB_DISTRICT), state, TFB_DISTRICT))

saveRDS(twr, 'towers.rds')






