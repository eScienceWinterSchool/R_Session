# someSetup ----
# github data folder:
link1="https://github.com/profemagallanes-unmsm/"
link2="FGV_Python_prepublish/raw/main/DataFiles/"
where=paste0(link1,link2)

# collecting_DataMap ----

fileMapPoints=paste0(where,"calls911_geo.geojson")
fileMapPolygons=paste0(where,"MapSeattle.geojson")
library(sf)
events=read_sf(fileMapPoints)
city=read_sf(fileMapPolygons)


# theMapPlot ----
library(ggplot2)
base=ggplot(city) + geom_sf() + theme_light()
eventsByTime=base + geom_sf(data = events,
                            size=0.5,
                            aes(color=nightTime)) 
eventsByTime + labs(title = "Calls to 911 by time of day")

