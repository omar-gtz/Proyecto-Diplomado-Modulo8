library(sf)
library(dplyr)

# 1. Descargar el paquete de mapas de amCharts (versión 4.1.32)
url <- "https://registry.npmjs.org/@amcharts/amcharts4-geodata/-/amcharts4-geodata-4.1.32.tgz"
download.file(url, "amcharts_geodata.tgz", mode = "wb")

# 2. Extraer solo el mapa de México y la licencia
untar("amcharts_geodata.tgz",
      files = c("package/json/mexicoHigh.json", "package/LICENSE"))

# 3. Leer, quedarse con nombre y clave ISO, y guardar
mapa <- read_sf("package/json/mexicoHigh.json") %>%
  select(NOMBRE = name, ISO = id)

st_write(mapa, "mexico_estados.geojson", delete_dsn = TRUE)

# Revisión rápida: debe dibujar los 32 estados
plot(st_geometry(mapa))