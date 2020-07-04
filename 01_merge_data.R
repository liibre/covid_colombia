# leyendo el corregido de ambos y juntando para hacer mapas
library(dplyr)
library(sf)
library(tmap)
library(readr)

mpos_shp <- read_sf("./data/municipios.shp")
deptos_shp <- read_sf("./data/departamentos.shp")
datos <- read_csv("./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

all(mpos_shp$depto %in% datos$depto)
all(mpos_shp$municipio %in% datos$`Ciudad de ubicación`)
all(datos$`Ciudad de ubicación` %in% mpos_shp$municipio)

setdiff(datos$depto, mpos_shp$depto)
setdiff(mpos_shp$depto, datos$depto)

#bien


# Diferentes escalas de plots deben poder ser producidas


# Escala depto. última fecha

lastFN <- datos %>%
  slice(which.max(`Fecha de notificación`)) %>%
  pull(`Fecha de notificación`)


# casos acumulados por departamento
Casos_acum_depto <- datos %>%
  group_by(depto) %>%
  summarise(n_casos = n())
names(Casos_acum_depto)


#El mapa más simple sprimer mapa, casos acumulados por departamento
Casos_depto <- deptos_shp %>%
  merge(Casos_acum_depto)

names(Casos_depto)
casos_acumulado_por_depto <- tm_shape(Casos_depto) +
  tm_borders() +
  tm_fill(col = "n_casos") +
  # tm_symbols(size = "n_casos",
  #            col = "red",
  #            border.col = "red",
  #            scale = 2,
  #            alpha = 0.5)
  NULL
casos_acumulado_por_depto


# casos acumulados por municipio ¡¡¡pero necesitamos filtrar por departamentos!!!


Casos_acum_mpo <- datos %>%
  group_by(`Ciudad de ubicación`) %>%
  summarise(n_casos = n())


Casos_mpo <- mpos_shp %>% merge(Casos_acum_mpo)
###estos mapas están pesados, no lo ruedes
#Tratar así:
Cundinamarca <- Casos_mpo %>%
  filter(depto == "Cundinamarca")
#casos_acumulado_por_mpo <- Cundinamarca %>%
 # tm_shape(Casos_mpo) +
#tm_fill(col = "n_casos") +



# Casos más recientes por fecha
CasosFNmax <- datos %>%
  filter(`Fecha de notificación` == lastFN-1) %>%
  group_by(`Ciudad de ubicación`, depto) %>%
  summarise(n_casos = n()) %>%
  mutate(FN = lastFN-1)

CasosFNmax_depto <- datos %>%
  filter(`Fecha de notificación` == lastFN-2) %>%
  group_by(depto) %>%
  summarise(n_casos = n()) %>%
  mutate(FN = lastFN-2)



#### esto es viejo
tidyr::pivot_longer(data = col, cols = starts_with("F"),names_to = "Evento", values_to = "Fechas") %>%
  group_by(Evento) %>%
  add_tally() %>% arrange(Fechas) %>%
  mutate(acum = cumsum(n)) %>% ungroup() %>%
  filter(Evento %in% c("FIS", "Fecha de notificación", "Fecha diagnostico")) %>%
  ggplot(aes(Fechas, n, fill = Evento)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  NULL
names(col)
col <- col %>%
  dplyr::filter(!is.na(FIS))




