library(dplyr)
library(readr)
library(stringr)
library(colmaps)
library(sf)

#usa municipios y departamentos de colmaps (son los shapes)
#colmaps::municipios
#colmaps::departamentos

# lee casos colombia
#- x ö necesitamos hacerle scrapping a esto, buscar

datos <- readr::read_csv("https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD")

#son un montón de asintomáticos: sin fecha de inicio de los síntomas
datos %>%
  count(FIS == "Asintomático", FIS) %>%
  arrange(desc(FIS))
datos %>%
  count(is.na(`Fecha diagnostico`))
#
problems(datos)

# en las columnas de fecha los asintomáticos están en texto, tenemos que remplazarlo
datos <- datos %>%
  mutate(`FIS` = str_replace(string = `FIS`,
                             pattern = "Asintomático",
                             replacement = "")) %>%
  #igual recuperados, la fecha de muerte no existe
  mutate(`Fecha de muerte` = str_replace(string = `Fecha de muerte`,
                                         pattern = "-   -",
                                         replacement = ""))

mpos_shp <- read_sf("./data/municipios.shp")

setdiff(unique(datos$`Ciudad de ubicación`), unique(mpos_shp$municipio))

# siempre vienen mal
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Villa de de Ubaté"] <- "Villa de San Diego de Ubaté"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Villa rica"] <- "Villa Rica"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "El Cantón San Pablo"] <- "El Cantón del San Pablo"#ya
#Coloso es Colosó, error en la planilla del INS
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Coloso"] <- "Colosó"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "San Benito abad"] <- "San Benito Abad"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "San Pedro los Milagros"] <- "San Pedro de los Milagros"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "San José la Montaña"] <- "San José de la Montaña"#ya

setdiff(unique(datos$`Ciudad de ubicación`), unique(mpos_shp$municipio))

#departamentos y distritos especiales
setdiff(mpos_shp$depto, datos$`Departamento o Distrito`)
distritos_especiales <- setdiff(datos$`Departamento o Distrito`, mpos_shp$depto)
datos <- datos %>%
  mutate(depto =
           case_when(
             !`Departamento o Distrito` %in% distritos_especiales ~ `Departamento o Distrito`,
             `Departamento o Distrito` == "Cartagena D.T. y C." ~ "Bolívar",
             `Departamento o Distrito` == "Barranquilla D.E." ~ "Atlántico",
             `Departamento o Distrito` == "Santa Marta D.T. y C." ~ "Magdalena",
             `Departamento o Distrito` == "Buenaventura D.E." ~ "Valle del Cauca"))
setdiff(mpos_shp$depto, datos$depto)
write_csv(datos, "./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")
