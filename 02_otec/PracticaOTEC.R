# Práctica: OTEC
library(raster)
library(rgdal)
library(sf)
library(tmap)

# Polígonos estatales
estados <- read_sf("ESTADOS/ESTADOS.shp")

# Batimetría del Mar Caribe
# Fuente: NOAA
batimetria <- raster("etopo1_bedrock.tif")

pais <- st_union(estados)
buffer <- st_buffer(pais, dist=15000)

buffer 

nivel <- calc(batimetria, function(x) { if (x == -700 || x == -800 || x == -900 || x == -1000) x else 0 })

tm_shape(nivel) + 
  tm_raster() + 
  tm_shape(pais) + 
  tm_polygons(alpha = 0.5) + 
  tm_shape(buffer) +
  tm_polygons(alpha = 0.5)
