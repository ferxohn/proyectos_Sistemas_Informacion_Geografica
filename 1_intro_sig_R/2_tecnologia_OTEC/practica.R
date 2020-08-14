# Práctica 2: Tecnología OTEC
# 
# Objetivo de la práctica: Localizar sitios en el Mar Caribe con potencial para
# instalación de tecnología OTEC.
# 
# Nota: La tecnología OTEC permite generar energía eléctrica en zonas marinas
# aprovechando la diferencia de temperatura entre la superficie y el fondo marino.
# Para ello es necesario ubicar sitios con profundidad entre 700 y 1000 metros
# ubicados a menos de 15 km de la costa.
# 
# Más información:
# https://cemieoceano.mx/energia-gradientes-termicos.html
library(sf)
library(rgdal)
library(gdalUtils)
library(dplyr)
library(cartography)
library(tmap)
setwd("1_intro_sig_R/2_tecnologia_OTEC/")

# Polígonos estatales
# Fuente: INEGI
estados <-
  st_as_sf(readOGR(
    dsn = "ESTADOS",
    layer = "ESTADOS",
    encoding = "UTF-8"
  ))

# Disolución de los polígonos estatales para obtener la forma del país
pais <- st_union(estados)

# Buffer del país con una anchura de 15 km
buffer <- st_buffer(pais, dist = 15000)

# Batimetría del Mar Caribe:
# Cálculo de las curvas de nivel cada 100 metros
# Fuente: NOAA
gdal_contour(
  a = "elev",
  i = 100,
  src_filename = "etopo1_bedrock.tif",
  dst_filename = "curvas_nivel/curvas_nivel.shp"
)
curvas_nivel <- read_sf("curvas_nivel/curvas_nivel.shp")

# Obtención del bbox del Mar Caribe
mar_caribe <- curvas_nivel %>%
  st_bbox()

# Filtro: Zonas con profundidad entre 700 y 1000 metros
zonas_filtradas <- curvas_nivel %>%
  filter(elev <= -700 & elev >= -1000) %>%
  # Transformación de las zonas filtradas al CRS usado en los polígonos de los estados
  st_transform(crs = st_crs(estados))

# Intersección de las zonas con profudidad entre 700 y 100 metros con el buffer
zonas_potenciales <- st_intersection(buffer, zonas_filtradas)

# Mapa base de OpenStreetMap
mar_caribe.osm <- getTiles(
  x = curvas_nivel,
  type = "osmgrayscale",
  zoom = 7,
  crop = TRUE
)

# Creación del PDF con el mapa temático
pdf("zonas_potenciales.pdf", width = 5.83, height = 7)
tm_shape(mar_caribe.osm) +
  tm_rgb() +
  tm_shape(buffer) +
  tm_polygons() +
  tm_shape(pais) +
  tm_polygons(col = "#f2f2f2", border.alpha = 0.5) +
  tm_shape(zonas_potenciales) +
  tm_lines(col = "blue",
           lwd = 4) +
  tm_compass(
    type = "rose",
    text.size = 1,
    size = 2,
    position = c("right", "top")
  ) +
  tm_layout(
    frame = TRUE,
    frame.double.line = TRUE,
    asp = NA,
    main.title = "Zonas con potencial para\ninstalar tecnología OTEC",
    main.title.size = 1,
    main.title.position = c("center", "top"),
  ) +
  tm_credits(
    "Autor: Fernando Gómez Perera\nFuentes: NOAA (s.f.)",
    size = 1,
    align = "right",
    position = c("right", "bottom")
  )
dev.off()

setwd("../../")
