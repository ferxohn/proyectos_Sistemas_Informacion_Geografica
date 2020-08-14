# Práctica: Incidencia Delictiva Nacional
# 
# Objetivo de la práctica: Construir mapas temáticos que muestren la incidencia
# delictiva en los estados de la República Mexicana durante los últimos años.
library(RODBC)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(cartography)
setwd("sistemas_informacion_geografica/incidencia_delictiva_nacional/")

# Incidencia delictiva del fuero común
# Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública
# 
# Nota: Para poder usar esta función es necesario tener instalado el "Componente
# redistribuible del motor de base de datos de Microsoft Access 2010", el cual
# se puede descargar desde el siguiente enlace:
# https://www.microsoft.com/en-US/download/details.aspx?id=13255
seg_pub_db <- odbcConnectExcel2007("Estatal-Delitos - diciembre 2019.xlsb")

seg_pub <- as_tibble(sqlFetch(seg_pub_db, "Hoja1$")) %>%
  mutate_at(vars(Clave_Ent), list(~as.character(.))) %>%
  mutate_if(is.numeric, as.integer) %>%
  mutate_at(vars(Clave_Ent), list(~str_pad(., width = 2, side = "left", pad = "0")))

odbcClose(seg_pub_db)
rm(seg_pub_db)

# Tamaño de población por estado
# Fuente: INEGI Encuesta Intercensal 2015
inegi <- read_excel("01_poblacion.xls", sheet = 4, range = "A7:E3573")
  
# Polígonos estatales
# Fuente: CONABIO
polig_est <- read_sf("destdv250k_2gw/destdv250k_2gw.shp") %>%
  select(-one_of("AREA", "PERIMETER", "COV_", "COV_ID", "CAPITAL", "RASGO_GEOG")) %>%
  rename(Entidad = ENTIDAD, Clave_Ent = NUM_EDO) %>%
  group_by(Clave_Ent, Entidad) %>%
  summarise() %>%
  mutate_at(vars(Entidad), list(~str_to_title(., locale = "es")))

# Filtro: Homicidios dolosos por arma de fuego
# Datos agrupados por año y por entidad
hom_dol_arma_fuego <- seg_pub %>%
  # Obtención de los homicidios dolosos por arma de fuego
  filter(`Subtipo de delito` == "Homicidio doloso", Modalidad == "Con arma de fuego") %>%
  select(-one_of("Bien jurídico afectado", "Tipo de delito", "Subtipo de delito", "Modalidad")) %>%
  pivot_longer(
    -one_of("Año", "Clave_Ent", "Entidad"),
    names_to = "Mes",
    values_to = "Total"
  ) %>%
  # Agrupación de los datos por año y por entidad
  group_by(`Año`, Clave_Ent, Entidad) %>%
  summarise_at(vars(Total), list(~sum(.))) %>%
  rename(Hom_Dol_Arma_Fuego = Total)

# Filtro: Tamaño de población por entidad
tam_pob <- inegi %>%
  filter(
    `Entidad federativa` != "Estados Unidos Mexicanos",
    Sexo == "Total",
    `Grandes grupos de edad` == "Total",
    Estimador == "Valor"
  ) %>%
  separate(
    `Entidad federativa`, 
    into = c("Clave_Ent", "Entidad"),
    sep = 3
  ) %>%
  mutate_at(vars(Clave_Ent), list(~str_trim(.))) %>%
  rename(Poblacion = `Población total`) %>%
  select(-one_of("Sexo", "Grandes grupos de edad", "Estimador"))

# Cálculo de la tasa de homicidios dolosos con arma de fuego por entidad por año
tasa_hom <- hom_dol_arma_fuego %>%
  full_join(tam_pob, by = "Clave_Ent") %>%
  mutate(Tasa_Hom = Hom_Dol_Arma_Fuego / Poblacion * 100000) %>%
  select(`Año`, Clave_Ent, Tasa_Hom)

# Unión con los poligonos estatales
sf_tasa_hom <- polig_est %>%
  ungroup() %>%
  full_join(tasa_hom, by = "Clave_Ent") %>%
  select(`Año`, Clave_Ent, Entidad, Tasa_Hom)

### Creación de los mapas temáticos ###

# Mapa base de OpenStreetMap
mex.osm <- getTiles(
  x = polig_est,
  type = "osm",
  zoom = 5,
  crop = TRUE
)

# División de la información por años
tasa_hom_anio <- list(
  "2015" = filter(sf_tasa_hom, `Año` == 2015),
  "2016" = filter(sf_tasa_hom, `Año` == 2016),
  "2017" = filter(sf_tasa_hom, `Año` == 2017),
  "2018" = filter(sf_tasa_hom, `Año` == 2018),
  "2019" = filter(sf_tasa_hom, `Año` == 2019)
)

# Cortes y paleta de colores para el Choropleth
pal <- carto.pal(pal1 = "green.pal", n1 = 1, pal2 = "orange.pal", n2 = 9, transparency = FALSE)
breaks <- getBreaks(seq(0, 100, by = 10), n = 10, method = "equal")

# Creación del PDF con los 5 mapas temáticos
pdf("inc_delic_anio_mex.pdf", width = 7, height = 5.3)
for (info_anio in tasa_hom_anio) {
  # Creación del mapa temático
  tilesLayer(x = mex.osm)
  choroLayer(
    x = info_anio,
    var = "Tasa_Hom",
    breaks = breaks,
    col = pal,
    border = "black",
    lwd = 0.2,
    legend.pos = "n",
    add = TRUE
  )
  legendChoro(
    pos = "left",
    title.txt = "Tasa de\nhomicidios",
    breaks = breaks,
    col = pal,
    nodata = FALSE
  )
  layoutLayer(
    title = paste0("Tasa de homicidios dolosos por arma de fuego (", unique(info_anio$`Año`), ")"),
    sources = "Fuentes: INEGI (2016) y Secretariado Ejecutivo del SNSP (2020)\n© OpenStreetMap contributors.\nTiles style under CC BY-SA, www.openstreetmap.org/copyright.",
    author = paste0("Por: Fernando Gómez Perera, usando cartography ", packageVersion("cartography")),
    frame = TRUE, north = TRUE, tabtitle = TRUE
  )
}
dev.off()

setwd("../../")
