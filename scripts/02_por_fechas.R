library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(zoo)

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
  summarise(n = n()) %>%
  mutate(mm = zoo::rollmean(n, k = 5, na.pad = TRUE, align = "right"))

  ggplot(datos_fecha, aes(x = data_final, y = n)) +
  geom_line(col = "grey") +
  geom_line(aes(y = mm, col = "media móvil (5 días)"), lwd = 1) +
  theme_minimal() +
    scale_color_brewer(type = "qual") +
    theme(legend.title = element_blank(),
          legend.position = c(0.2, 0.8)) +
    ggtitle("Número de casos por día, por fecha de inicio de los síntomas (FIS)") +
    xlab("Fecha de inicio de los síntomas (FIS)") +
    ylab("Número de casos")

ggsave("figs/casos_diarios.png")

# muertes
names(datos)
datos %>% count(!is.na(`Fecha de muerte`))
muertes_fecha <-
  datos %>%
  filter(!is.na(`Fecha de muerte`)) %>%
  group_by(data_final) %>%
  summarise(n = n()) %>%
  mutate(mm = zoo::rollmean(n, k = 5, na.pad = TRUE, align = "right"))
#plot----
muertes_fecha %>%
  ggplot(aes(x = data_final, y = n)) +
  geom_line(col = "grey") +
  geom_line(aes(y = mm, col = "media móvil (5 días)"), lwd = 1) +
  theme_minimal() +
  scale_color_brewer(type = "qual") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8)) +
  ggtitle("Número de fallecimientos por día, por fecha de inicio de los síntomas (FIS)") +
  xlab("Fecha de inicio de los síntomas (FIS)") +
  ylab("Número de casos")
ggsave("figs/muertes_diarias.png")


