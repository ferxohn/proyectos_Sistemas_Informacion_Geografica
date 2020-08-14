# Práctica 3: Mapas de Calor
# 
# Objetivo de la práctica: Construir mapas de calor para mostrar la distribución
# de incidentes delictivos en la ciudad de San Diego, California. Específicamente,
# el hurto con allanamiento de morada (burglary), el cual puede presentar patrones
# diurnos y nocturnos distintos.
library(sf)
library(raster)
library(spatialEco)
library(dplyr)
library(tmap)
setwd("1_intro_sig_R/3_heatmaps/")

# Mapa de San Diego
# Fuente: City of San Diego Open Data Portal
san_diego <-
  read_sf("san_diego_boundary_datasd") %>%
  st_transform(crs = 4326)

# Información sobre hurto con allanamiento de morada
# Fuente: San Diego Regional Data Library
burglary <-
  read_sf("crime-incidents-2010.shapefile") %>%
  filter(type == "BURGLARY") %>%
  st_transform(crs = 4326)

# División por día y noche
burglary_day <- burglary %>%
  filter(is_night == 0) %>%
  as_Spatial()

burglary_night <- burglary %>%
  filter(is_night == 1) %>%
  as_Spatial()

# Cálculo de los heatmaps por día y noche
heatmap_day <-
  sp.kde(x = burglary_day,
         bw = 0.05,
         nr = 1043,
         nc = 1587) %>%
  mask(as_Spatial(san_diego))
names(heatmap_day) <- "Día"

heatmap_night <-
  sp.kde(x = burglary_night,
         bw = 0.05,
         nr = 1043,
         nc = 1587) %>%
  mask(as_Spatial(san_diego))
names(heatmap_night) <- "Noche"
extent(heatmap_day) <- heatmap_night

heatmaps <- stack(heatmap_day, heatmap_night)

# Límites de los mapas
san_diego_bbox <- st_bbox(san_diego)
san_diego_bbox[["xmin"]] <- san_diego_bbox[["xmin"]] - 0.2
san_diego_bbox[["xmax"]] <- san_diego_bbox[["xmax"]] - 0.2

# Intervalos a usar para los colores del heatmap
breaks <- seq(0, 30, by = 3)

# Creación del PDF con los heatmaps
pdf("heatmaps_san_diego.pdf", width = 7, height = 5.3)
tm_shape(heatmaps, bbox = san_diego_bbox) +
  tm_raster(
    palette = "-Spectral",
    breaks = breaks,
    title = "Total",
    legend.format = list(text.separator = "a")
  ) +
  tm_compass(text.size = 0.8,
             size = 1.5) +
  tm_credits(
    "Los mapas permiten visualizar que los robos son más frecuentes por las noches que durante el día,\nsobre todo en las zonas donde se presentan más robos durante el día.
    Autor: Fernando Gómez Perera
    Fuentes: City of San Diego Open Data Portal, San Diego Regional Data Library",
    size = 1,
    align = "left",
    position = c("left", "bottom")
  ) +
  tm_layout(
    frame.double.line = TRUE,
    compass.type = "arrow",
    legend.outside = TRUE,
    legend.outside.position = "right",
    panel.labels = c("Día", "Noche"),
    title.snap.to.legend = FALSE,
    main.title = "Hurto con allanamiento de morada - San Diego (2010)",
    main.title.position = "center",
    main.title.size = 1,
    attr.outside = TRUE,
    attr.outside.position = "bottom"
  )
dev.off()

setwd("../../")
