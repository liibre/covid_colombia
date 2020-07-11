# leyendo el corregido de ambos y juntando para hacer mapas
library(dplyr)
library(sf)
library(tmap)
library(readr)
library(spData)


# os países fronteiriços ----
front <- world %>%
  filter(name_long %in% c("Brazil",
                          "Colombia",
                          "Ecuador",
                          "Panama",
                          "Peru",
                          "Venezuela"))

mpos_shp <- read_sf("./data/municipios.shp")
deptos_shp <- read_sf("./data/departamentos.shp")

datos <- read_csv("./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

all(mpos_shp$depto %in% datos$depto)
all(mpos_shp$municipio %in% datos$`Ciudad de ubicación`)
all(datos$`Ciudad de ubicación` %in% mpos_shp$municipio)

setdiff(datos$depto, mpos_shp$depto)
setdiff(mpos_shp$depto, datos$depto)


# casos totales por departamento ----
casos_totales_depto <- datos %>%
  group_by(depto) %>%
  summarise(n_casos = n())
names(casos_totales_depto)

casos_depto <- deptos_shp %>%
  left_join(casos_totales_depto) %>%
  mutate(n_casos_log = log(n_casos + 1))

deptos_map <-
  tm_shape(casos_depto) +
  #tm_borders(col = "grey") +
  tm_fill(col = "n_casos_log") +
  tm_style("col_blind") +
  tm_shape(front) +
  tm_borders(col = "grey") +
  tm_layout(legend.bg.color = "white") +
  NULL

deptos_map

# casos totales por municipio ----

casos_totales_mpo <- datos %>%
  rename(municipio = `Ciudad de ubicación`) %>%
  group_by(municipio) %>%
  summarise(n_casos = n()) %>%
  mutate(n_casos_log = log(n_casos + 1))

mpos_shp <- mpos_shp %>%
  left_join(casos_totales_mpo)

mpos_map <- mpos_shp %>%
  tm_shape() +
  tm_borders(lwd = 0) +
  tm_fill(col = "n_casos_log") +
  #tm_shape(deptos_shp) +
  #tm_borders(col = "white") +
  #tm_facets(by = "depto", free.coords = TRUE) +
  tm_style("col_blind") +
  tm_shape(front) +
  tm_borders(col = "grey") +
  tm_layout(legend.position = c("left", "top")) +
  NULL

mpos_map


