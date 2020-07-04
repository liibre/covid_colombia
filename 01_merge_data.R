# leyendo el corregido de ambos y juntando para hacer mapas
library(sf)
library(tmap)
library(readr)
library(ggplot2)
mpo_shp <- read_sf("./data/municipios.shp")
depto_shp <- read_sf("./data/departamentos.shp")
datos <- read_csv("./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

all(mpo_shp$depto %in% datos$depto)
all(mpo_shp$municipio %in% datos$`Ciudad de ubicación`)
all(datos$`Ciudad de ubicación` %in% mpo_shp$municipio)

# Diferentes escalas de plots deben poder ser producidas

#vector de fechas
names(datos)
datos %>%
  group_by(`Fecha de notificación`) %>%
  summarise(Not_dia = n()) %>%
ggplot(aes(x = `Fecha de notificación`, y = Not_dia)) +
  geom_line() +
  theme_minimal()

datos %>%
  group_by(`Fecha diagnostico`) %>%
  summarise(Not_dia = n()) %>%
ggplot(aes(x = `Fecha diagnostico`, y = Not_dia)) +
  geom_line() +
  theme_minimal()

dados_FIS <- datos %>%
  group_by(`FIS`) %>%
  summarise(Not_dia = n()) %>%
  arrange(desc(Not_dia)) %>%
  filter(!is.na(FIS))
dados_FIS %>%
  ggplot(aes(x = `FIS`, y = Not_dia)) +
  geom_line() +
  theme_minimal()

count(datos, `FIS`) %>% arrange(desc(n))
count(datos, `Fecha diagnostico`) %>% filter(is.na(n))
count(datos, `Fecha de notificación`) %>% arrange(desc(n))
names(datos)
FIS_diag <- datos %>%
  select(FIS, `Fecha diagnostico`) %>%
  filter(complete.cases(.)) %>%
  data.frame(.)

head(FIS_diag)
trim.now = 10
window = 40
library(NobBS)
dados.now <- NobBS(
  data = FIS_diag,
  now = max(FIS_diag$Fecha.diagnostico), #- trim.now,
  onset_date = "FIS",
  report_date = "Fecha.diagnostico",
  units = "1 day",
  moving_window = window)

ggplot(dados.now$estimates, aes(x = onset_date, y = estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
                  fill = RColorBrewer::brewer.pal(3, "Set1")[2],
              alpha = 0.5) +
  geom_line() +
  geom_line(aes(y = n.reported), col = "red") +
  theme_minimal() +
NULL
head(dados.now$estimates)

  ggplot(dados_FIS, aes(x = FIS, y = Not_dia)) +
  geom_line(col = "dodgerblue") +
  geom_ribbon(data = dados.now$estimates, aes(x = onset_date, y = n.reported, ymin = lower, ymax = upper),
              fill = RColorBrewer::brewer.pal(3, "Set1")[2],
              alpha = 0.5) +
  geom_line(data = dados.now$estimates, aes(x = onset_date, y = estimate), col = "black") +
  theme_minimal()
  #por eso haces rolling medias

# Escala depto. ültima fecha

lastFN <- datos %>%
  slice(which.max(`Fecha de notificación`)) %>%
  pull(`Fecha de notificación`)


# casos acumulados por departaento
Casos_acum_depto <- datos %>%
  group_by(depto) %>%
  summarise(n_casos = n())
names(Casos_acum_depto)


Casos_depto <- deptos_shp %>% merge(Casos_acum_depto)

names(Casos_depto)
casos_acumulado_por_depto <- tm_shape(Casos_depto) +
  tm_borders() +
  #tm_fill(col = "n_casos") +
  tm_symbols(size = "n_casos",
             col = "red",
             border.col = "blue",
             scale = 2,
             alpha = 0.5)
casos_acumulado_por_depto
mapa + tm_fill(alpha = .7)

# casos acumulados por municipio
Casos_acum_depto <- datos %>%
  group_by(depto) %>%
  summarise(n_casos = n())
names(Casos_acum_depto)

deptos_shp <- departamentos %>% st_as_sf()

Casos_depto <- deptos_shp %>% merge(Casos_acum_depto)

names(Casos_depto)
casos_acumulado_por_depto <- tm_shape(Casos_depto) +
  tm_borders() +
  #tm_fill(col = "n_casos") +
  tm_symbols(size = "n_casos",
             col = "red",
             border.col = "blue",
             scale = 2,
             alpha = 0.5)
casos_acumulado_por_depto
mapa + tm_fill(alpha = .7)



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



####
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



###
dados <- col %>% select(FIS, `Fecha de notificación`) %>% mutate(n = 1)

library(NobBS)

dados.now <- NobBS(
  data = dados,
  now = max(dados$FIS),
  onset_date = "FIS",
  report_date = "Fecha de notificación",
  units = "1 day",
  moving_window = window)
