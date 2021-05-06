

# map icons
all_icons <- awesomeIconList(
  'Smoke Location' = makeAwesomeIcon(icon= 'fire', markerColor = 'orange', iconColor = '#FFFFFF', library = "fa"),
  'Fire' = makeAwesomeIcon(icon = 'fire', markerColor = 'darkred', iconColor = '#FFFFFF', library = "fa"),
  'Planned Burn' = makeAwesomeIcon(icon = 'fire', markerColor = 'purple', iconColor = '#FFFFFF', library = "fa"),
  'Fire Lookout' = makeAwesomeIcon(icon = 'binoculars', markerColor = 'green', iconColor = '#FFFFFF', library = "fa"),
  'Mobile Location' = makeAwesomeIcon(icon = 'truck', markerColor = 'blue', iconColor = '#FFFFFF', library = "fa")
)

# legend html generator
# Thanks to Andrew Reid on StackOverflow https://stackoverflow.com/a/47107058/1498485
markerLegendHTML <- function(IconSet) {
  
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h3 style='padding-top:0; padding-bottom:10px; margin-top: 11px;'> Map Legend </h2>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='color: #FFFFFF; margin-left: 0px; margin-top: 11px;' class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 5px; display: inline-block;' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}
