library(readr)
library(ggplot2)
library(dplyr)

datos <- read_csv("./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

#### mirando fechas
tidyr::pivot_longer(data = datos, cols = starts_with("F"),names_to = "Evento", values_to = "Fechas") %>%
  group_by(Evento) %>%
  add_tally() %>% arrange(Fechas) %>%
  mutate(acum = cumsum(as.numeric(n))) %>% ungroup() %>%
  filter(Evento %in% c("FIS", "Fecha de notificación", "Fecha diagnostico")) %>%
  ggplot(aes(Fechas, n, fill = Evento)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  NULL

FIS_casos <- tidyr::pivot_longer(data = datos, cols = starts_with("F"),names_to = "Evento", values_to = "Fechas") %>%
  filter(Evento == "FIS") %>%
  add_tally() %>% arrange(Fechas) %>%
  mutate(acum = cumsum(as.numeric(n))) %>%
  ggplot(aes(Fechas, n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Fecha de inicio de los síntomas") +
  NULL
FIS_casos


lastFN <- datos %>%
  slice(which.max(`FIS`)) %>%
  pull(`FIS`)


CasosFNmax <- datos %>%
  filter(`FIS` == lastFN - 1) %>%
  group_by(`Ciudad de ubicación`, depto) %>%
  summarise(n_casos = n()) %>%
  mutate(FN = lastFN - 1)

CasosFNmax_depto <- datos %>%
  filter(`Fecha de notificación` == lastFN-2) %>%
  group_by(depto) %>%
  summarise(n_casos = n()) %>%
  mutate(FN = lastFN - 2)

names(datos)
datos_not <- datos %>%
  group_by(`Fecha de notificación`) %>%
  # Diferentes escalas de plots deben poder ser producidas
  summarise(Not_dia = n())


datos_diag <- datos %>%
  group_by(`Fecha diagnostico`) %>%
  summarise(Diag_dia = n())

  ggplot(datos_diag, aes(x = `Fecha diagnostico`, y = Diag_dia)) +
  geom_line() +
  theme_minimal() +
    geom_line(data = datos_not, aes(x = `Fecha de notificación`, y = Not_dia), col = "red")

datos <-
  datos %>%
    mutate(data_final =
             case_when(`Fecha de notificación` > `Fecha diagnostico` ~ `Fecha de notificación`,
                       `Fecha de notificación` <= `Fecha diagnostico` ~ `Fecha diagnostico`))

datos_fecha <- datos %>%
  group_by(data_final) %>%
  summarise(n = n())

  ggplot(datos_fecha, aes(x = data_final, y = n)) +
  geom_line() +
  theme_minimal()
