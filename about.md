

## About

In the Australian state of Victoria on a hot, dry summer day fire can 
spread at up to 10 km/h in forest and up to 20 km/h in grassland. 
The timeliness of the initial response to a fire is critical to containment 
and reducing the severity and impact of the fire. 

Victoria has a network of almost 90 active fire watch towers. When smoke is sighted, these towers 
use a triangulation method to narrow in on the exact location. 

In this application when you select 2 or more towers, lines are drawn from 
each tower at the given bearing will intersect indicating the location of the smoke. 

This application is intended to replicate and simplify the manual 
triangulation process, while at the same time incorporate modern 
reverse geocoding capabilities that can convert a location into an address.


## Data Sources

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


## Improvements

All code for this application is available on [Github](https://github.com/mrjoh3/trilocate)
If you experience errors or wish to suggest improvements, please submit a 
[Github Issue](https://github.com/mrjoh3/trilocate/issues).