# leyendo el corregido de ambos y juntando para hacer mapas
library(dplyr)
library(sf)
library(tmap)
library(readr)
library(spData)
library(ggplot2)

# os países fronteiriços innecesario em modo view----

# shapes y datos----
mpos_shp <- read_sf("../data/municipios.shp")
deptos_shp <- read_sf("../data/departamentos.shp")

datos <- read_csv("../data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

# all(mpos_shp$depto %in% datos$depto)
# setdiff(mpos_shp$municipio, datos$`Ciudad de ubicación`)
# all(datos$`Ciudad de ubicación` %in% mpos_shp$municipio)
#
# setdiff(datos$depto, mpos_shp$depto)
# setdiff(mpos_shp$depto, datos$depto)


# casos totales por departamento ----
casos_totales_depto <- datos %>%
  group_by(depto) %>%
  summarise(n_casos = n()) %>%
  mutate(n_casos_log = log(n_casos + 1))

deptos_shp <- deptos_shp %>%
  left_join(casos_totales_depto) %>%
  relocate(depto)


tmap_mode("plot")
deptos_map <-
  tm_shape(deptos_shp) +
  tm_polygons("n_casos",
              title = "Casos",
              lwd = 0,
              group = "depto",
              popup.vars = "n_casos") +
  tm_style("col_blind") +
  tm_shape(world) +
  tm_borders(col = "grey") +
  tm_layout(legend.bg.color = "white") +
  NULL
hist(deptos_shp$n_casos_log)
tmap_mode("plot")
deptos_map_log <-
  tm_shape(deptos_shp) +
  tm_polygons("n_casos_log",
              title = "Casos",
              lwd = 0,
              popup.vars = "n_casos",
              breaks = c(
                log(1),
                log(50),
                log(100),
                log(500),
                log(1000),
                log(5000),
                log(10000),
                log(50000),
                log(100000),
                log(5000000)
                ),
              labels = c(
                "0 a 50",
                "51 a 100",
                "101 a 500",
                "501 a 1.000",
                "1.001 a 5.000",
                "5.001 a 10.000",
                "10.001 a 50.000",
                "50.001 a 100.000",
                "> 100.000"

                )) +
  tm_style("col_blind") +
  tm_shape(world) +
  tm_borders(col = "grey") +
  tm_layout(legend.bg.color = "white") +
  NULL
deptos_map_log
tmap_save(filename = "./figs/deptos.png")
tmap_mode("view")
deptos_map_log

# casos totales por municipio ----

casos_totales_mpo <- datos %>%
  rename(municipio = `Ciudad de ubicación`) %>%
  group_by(municipio) %>%
  summarise(n_casos = n()) %>%
  mutate(n_casos_log = log(n_casos + 1))

mpos_shp <- mpos_shp %>%
  left_join(casos_totales_mpo) %>%
  mutate(nombre = paste(municipio, depto, sep = ",")) %>%
  relocate(nombre)

tmap_mode("plot")
mpos_map <-  mpos_shp %>%
  tm_shape() +
  tm_polygons("n_casos",
              title = "Casos",
              lwd = 0,
              popup.vars = "n_casos") +
  tm_style("col_blind") +
  tm_shape(world) +
  tm_borders(col = "grey") +
  tm_layout(legend.bg.color = "white") +
  NULL
mpos_map

tmap_mode("plot")
mpos_map_log <- mpos_shp %>%
  tm_shape() +
  tm_polygons("n_casos_log",
              title = "Casos",
              lwd = 0,
              popup.vars = "n_casos",
              breaks = c(
                log(1),
                log(50),
                log(100),
                log(500),
                log(1000),
                log(5000),
                log(10000),
                log(50000),
                log(100000),
                log(5000000)
              ),
              labels = c(
                "0 a 50",
                "51 a 100",
                "101 a 500",
                "501 a 1.000",
                "1.001 a 5.000",
                "5.001 a 10.000",
                "10.001 a 50.000",
                "50.001 a 100.000",
                "> 100.000"

              )) +
  tm_style("col_blind") +
  tm_shape(world) +
  tm_borders(col = "grey") +
  tm_layout(legend.bg.color = "white") +
  NULL

mpos_map_log
tmap_save(filename = "./figs/mpos.png")
tmap_mode("view")
mpos_map_log
