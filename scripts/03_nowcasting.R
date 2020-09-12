# nowcasting bayesiano con una ventana e un triming para evitar los días con rezago
library(NobBS)
library(readr)

datos <- read_csv("./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

datos <-
  datos %>%
  mutate(data_final =
           case_when(`Fecha de notificación` > `Fecha diagnostico` ~ `Fecha de notificación`,
                     `Fecha de notificación` <= `Fecha diagnostico` ~ `Fecha diagnostico`))
datos %>% count(`Fecha de notificación` < `Fecha diagnostico`)
datos %>% count(is.na(`Fecha de notificación`), is.na(`Fecha diagnostico`), is.na(FIS))

FIS_diag <- datos %>%
  select(FIS, data_final) %>%
  filter(complete.cases(.)) %>%
  data.frame() %>%
  mutate_all(.funs = function(x) as.Date(x))

FIS_diag_muertes <- datos %>%
  filter(!is.na(`Fecha de muerte`)) %>%
  select(FIS, data_final, `Fecha de muerte`, `fecha reporte web`) %>%
  filter(complete.cases(.)) %>%
  data.frame() %>%
  mutate_all(.funs = function(x) as.Date(x))

window <- 80
trim.now <- 10
dados.now <- NobBS(
  data = FIS_diag,
  now = max(FIS_diag$data_final) - trim.now,
  onset_date = "FIS",
  report_date = "data_final",
  units = "1 day",
  moving_window = window)

ggplot(dados.now$estimates, aes(x = onset_date, y = estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = RColorBrewer::brewer.pal(3, "Set1")[2],
              alpha = 0.5) +
  geom_line(aes(col= "Corregidos")) +
  geom_line(aes(y = n.reported, col = "Notificados")) +
  scale_colour_brewer(
    type = "qual",
    palette = 2,
    direction = 1,
    aesthetics = "colour"
  ) +
  theme_minimal() +
  xlab("Fecha de inicio de los síntomas") +
  ylab("Fecha de notificación") +
  theme(legend.title = element_blank()) +
  ggtitle(paste("Número de casos estimados por corrección de atrasos de notificación por nowcasting bayesiano (Mc Gough 2020).  \nVentana de", window, "días, omitiendo los", trim.now, "días más recientes",
                "
                \nDatos originales: Instituto Nacional de Salud.
                \nCrédito: Observatorio COVID-19BR https://covid19br.github.io/, ¡liibre!")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8))
ggsave("figs/Nowcasting.png")


## nowcasting muertes
dados_now_muertes <- NobBS(
  data = FIS_diag_muertes,
  now = max(FIS_diag$data_final) - trim.now,
  onset_date = "FIS",
  report_date = "data_final",
  units = "1 day",
  moving_window = window)

ggplot(dados_now_muertes$estimates, aes(x = onset_date, y = estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = RColorBrewer::brewer.pal(3, "Set1")[2],
              alpha = 0.5) +
  geom_line(aes(col= "Corregidos")) +
  geom_line(aes(y = n.reported, col = "Notificados")) +
  scale_colour_brewer(
    type = "qual",
    palette = 2,
    direction = 1,
    aesthetics = "colour"
  ) +
  theme_minimal() +
  xlab("Fecha de inicio de los síntomas") +
  ylab("Fecha de notificación") +
  theme(legend.title = element_blank()) +
  ggtitle(paste("Número de muertes estimadas por corrección de atrasos de notificación por nowcasting bayesiano (Mc Gough 2020).  \nVentana de", window, "días, omitiendo los", trim.now, "días más recientes",
                "
                \nDatos originales: Instituto Nacional de Salud.
                \nCrédito: Observatorio COVID-19BR https://covid19br.github.io/, ¡liibre!")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8))
ggsave("figs/Nowcasting_muertes.png")


# Distribución de los atrasos
FIS_diag %>% mutate(atraso = data_final - FIS) %>%
  ggplot(aes(x = atraso)) +
  geom_density()
ggsave("figs/distribuction_atrasos_casos.png")
FIS_diag_muertes %>%
  mutate(atraso = Fecha.de.muerte - FIS) %>%
  ggplot(aes(x = atraso)) +
  geom_density() +
  theme_minimal()
ggsave("figs/distribuction_atrasos_muertes.png")
