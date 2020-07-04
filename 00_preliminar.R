library(dplyr)
library(readr)
library(stringr)
library(colmaps)
library(sf)

#usa municipios y departamentos de colmaps (son los shapes)
colmaps::municipios
colmaps::departamentos

# lee casos colombia
#- [] ö necesitamos hacerle scrapping a esto, buscar

datos <- readr::read_csv("./data/Casos_positivos_de_COVID-19_en_Colombia.csv")

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
  #filter(FIS != "Asintomático",) %>%
  mutate(`FIS` = str_replace(string = `FIS`,
                             pattern = "Asintomático",
                             replacement = "")) %>%
  #igual recuperados, la fecha de muerte no existe
  mutate(`Fecha de muerte` = str_replace(string = `Fecha de muerte`,
                                         pattern = "-   -",
                                         replacement = "")) %>%
  #Hay fechas de diagnostico con SIN DATO en vez de na

  #modificamos las columnas de fecha para que sean fechas
  dplyr::mutate_at(.vars = vars(starts_with("F")),
                   .funs = function(x) lubridate::as_date(x, "%Y-%m-%d hh:mm:ss"))
#listo

# para hacer la tabla para el mapa necesitamos saber si los nombres de los municipios concuerdan ("baten")

setdiff(unique(datos$`Ciudad de ubicación`), unique(municipios@data$municipio))
#ok en principio municipios sin casos pero no tiene sentido que Bogotá esté ahí.
#toca formatar algunos municipios del shape :/
#PEEEERO
setdiff(unique(datos$`Ciudad de ubicación`), unique(municipios@data$municipio))
setdiff(unique(municipios@data$municipio), unique(datos$`Ciudad de ubicación`))


###limpiar a mano ambos y salvar
municipios@data$municipio[municipios@data$municipio == "Bogotá, D.c."] <- "Bogotá D.C."#ya
#los De están con mayúsucula
municipios@data$municipio[grepl(pattern = "De", x = municipios@data$municipio)]
municipios@data$municipio <- str_replace(string = municipios@data$municipio, pattern = "De", replacement = "de")
municipios@data$municipio[grepl(pattern = "De", x = municipios@data$municipio)]
#Ubaté tiene dos "de"
municipios@data$municipio <- str_replace(string = municipios@data$municipio, pattern = "De", replacement = "de")
municipios@data$municipio[grepl(pattern = "De", x = municipios@data$municipio)]
municipios@data$municipio[grepl(pattern = "de", x = municipios@data$municipio)]
#listo

setdiff(unique(datos$`Ciudad de ubicación`), unique(municipios@data$municipio))

#falta formatear algunos.
#La con mayúscula
# - [] ö necesito hacer esto con un grep pero no pude porque hay palabras con "la" y ^La$ no sirve y .+La tampoco
municipios$municipio[municipios$municipio == "María La Baja"] <- "María la Baja"
municipios$municipio[municipios$municipio == "Campo de La Cruz"] <- "Campo de la Cruz"
municipios$municipio[municipios$municipio == "Purísima de La Concepción"] <- "Purísima de la Concepción"

setdiff(unique(datos$`Ciudad de ubicación`), unique(municipios@data$municipio))
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Villa de de Ubaté"] <- "Villa de San Diego de Ubaté"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Villa rica"] <- "Villa Rica"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "El Cantón San Pablo"] <- "El Cantón del San Pablo"#ya
#Coloso es Colosó, error en la planilla del INS
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Coloso"] <- "Colosó"#ya

datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "San Benito abad"] <- "San Benito Abad"#ya


setdiff(unique(datos$`Ciudad de ubicación`), unique(municipios@data$municipio))
#listo

setdiff(unique(municipios@data$municipio), unique(datos$`Ciudad de ubicación`))
#aquí todavía puede haber errores o puede ser simplemente que no hay datos de covid
#falta corregir La" "Los" y necesito ayuda con esos greps o que hacer

#los datos de departamentooooos también tiene diferencias TANTO EN MUNICIPIOS
# COMO EN DEPARTAMENTOS
setdiff(municipios$depto, datos$`Departamento o Distrito`)
setdiff(departamentos$depto, datos$`Departamento o Distrito`)

#hay typos que hacen que sea diferente, toca remplazar a mano la primera vez y después salvar y no volver a tocar esto
municipios$depto[municipios$depto == "Bogotá, D. C."] <- "Bogotá D.C."
municipios$depto[municipios$depto == "Archipiélago De San Andrés, Providencia Y Santa Catalina"] <- "Archipiélago de San Andrés Providencia y Santa Catalina"
municipios$depto[municipios$depto == "Norte De Santander"] <- "Norte de Santander"
municipios$depto[municipios$depto == "Valle Del Cauca"] <- "Valle del Cauca"

#igual toca para departamentos

departamentos$depto[departamentos$depto == "Bogotá, D. C."] <- "Bogotá D.C."
departamentos$depto[departamentos$depto == "Archipiélago De San Andrés, Providencia Y Santa Catalina"] <- "Archipiélago de San Andrés Providencia y Santa Catalina"
departamentos$depto[departamentos$depto == "Norte De Santander"] <- "Norte de Santander"
departamentos$depto[departamentos$depto == "Valle Del Cauca"] <- "Valle del Cauca"

# faltan los distritos especiales... los datos del INS aceptan distritos especiales en vez de departamentos
setdiff(datos$`Departamento o Distrito`, municipios$depto)
#la tabla de departamentos del INS tiene en una categoría aparte cinco ciudades
# pero estas ciudades quedan en departamentos específicos, vamos a crear una
# columna depto para que sea solo el departamento y mantenemos la original con esa distinción

# no se van a poder mapear en paz - pero para tablas y figuras es importante
# separar esas ciudades del resto del departamento
distritos_especiales <- setdiff(datos$`Departamento o Distrito`, municipios$depto)

datos <- datos %>%
  mutate(depto =
           case_when(!`Departamento o Distrito` %in% distritos_especiales ~ `Departamento o Distrito`,
                     `Departamento o Distrito` == "Cartagena D.T. y C." ~ "Bolívar",
                     `Departamento o Distrito` == "Barranquilla D.E." ~ "Atlántico",
                     `Departamento o Distrito` == "Santa Marta D.T. y C." ~ "Magdalena",
                     `Departamento o Distrito` == "Buenaventura D.E." ~ "Valle del Cauca"
                     ))
count(datos, depto, `Departamento o Distrito`)
# toca salvar ambos corregidos
write_csv(datos, "./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

#toca tranformar municipios
municipios %>% st_as_sf() %>% st_write("./data/municipios.shp", delete_layer = TRUE)
departamentos %>% st_as_sf() %>% st_write("./data/departamentos.shp", delete_layer = TRUE)

