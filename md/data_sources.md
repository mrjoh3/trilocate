

Matching an address or location to coordinates is done using the open source
[Open Street Map (OSM) Nominatim](https://nominatim.openstreetmap.org) API 
made available via the [tmaptools](https://cran.r-project.org/web/packages/tmaptools/index.html) R package.
                        
All data used on this application is openly available via the links below: 

* Tower location and names are sourced from the 
[Geomark Point](https://discover.data.vic.gov.au/dataset/geomark-point) 
data set which contains the named locations of topographic features. 
* Current fire incidents and burn areas are sourced from a geojson hosted on Victoria's 
[Emergency Portal](https://www.emergency.vic.gov.au/public/osom-geojson.json).
* Victoria's Country Fire Authority (CFA) 
[Total Fire Ban Districts (TFB)](https://discover.data.vic.gov.au/dataset/country-fire-authority-cfa-total-fire-ban-districts-vicmap-admin)
are part of the Victorian governments Vicmap Admin series which contains administrative boundaries.