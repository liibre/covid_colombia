library(dplyr)
library(readr)
library(stringr)
library(colmaps)
library(sf)

#usa municipios y departamentos de colmaps (son los shapes)
mpos_shp <- read_sf("./data/municipios.shp")
deptos_shp <- read_sf("./data/departamentos.shp")

# lee casos colombia
#- [x] ö necesitamos hacerle scrapping a esto, buscar - ERA BAJAR DIRECTAMENTE, DUH

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
  #filter(FIS != "Asintomático",) %>%
  mutate(`FIS` = str_replace(string = `FIS`,
                             pattern = "Asintomático",
                             replacement = "")) %>%
  #igual recuperados, la fecha de muerte no existe
  mutate(`Fecha de muerte` = str_replace(string = `Fecha de muerte`,
                                         pattern = "-   -",
                                         replacement = ""))
#Hay fechas de diagnostico con SIN DATO en vez de na

# YA NO NECESITAMOS FORMATEAR FECHAS

# para hacer la tabla para el mapa necesitamos saber si los nombres de los municipios concuerdan ("baten")
# en este punto lo que importa es que cuadren con mpos_shp que ya está modificado


#LO QUE VIENE MAL DEL INS
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Villa de de Ubaté"] <- "Villa de San Diego de Ubaté"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Villa rica"] <- "Villa Rica"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "El Cantón San Pablo"] <- "El Cantón del San Pablo"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Coloso"] <- "Colosó"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "San Benito abad"] <- "San Benito Abad"#ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "San Pedro los Milagros"] <- "San Pedro de los Milagros" #ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "San José la Montaña"] <- "San José de la Montaña" #ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "Armero"] <- "Armero Guayabal" #ya
datos$`Ciudad de ubicación`[datos$`Ciudad de ubicación` == "El Litoral San Juan"] <- "El Litoral del San Juan" #ya

nao_tem <- setdiff(unique(datos$`Ciudad de ubicación`), unique(mpos_shp$municipio))
#listo

#aquí todavía puede haber errores o puede ser simplemente que no hay datos de covid
#falta corregir La" "Los" y necesito ayuda con esos greps o que hacer
#[x] quedó pero toca meterle reproducibilidad a esta parte.

#los datos de departamentooooos también tiene diferencias TANTO EN MUNICIPIOS
# COMO EN DEPARTAMENTOS

#hay typos que hacen que sea diferente, toca remplazar a mano la primera vez y después salvar y no volver a tocar esto
# municipios$depto[municipios$depto == "Bogotá, D. C."] <- "Bogotá D.C."
# municipios$depto[municipios$depto == "Archipiélago De San Andrés, Providencia Y Santa Catalina"] <- "Archipiélago de San Andrés Providencia y Santa Catalina"
# municipios$depto[municipios$depto == "Norte De Santander"] <- "Norte de Santander"
# unique(mpos_shp$depto) %>% View()
# mpos_shp$depto[mpos_shp$depto == "Archipiélago De San Andrés, Providencia Y Santa Catalina"] <- "Archipiélago de San Andrés Providencia y Santa Catalina"
# municipios$depto[municipios$depto == "Norte De Santander"] <- "Norte de Santander"
# municipios$depto[municipios$depto == "Valle Del Cauca"] <- "Valle del Cauca"
#
# #igual toca para departamentos
#
# departamentos$depto[departamentos$depto == "Bogotá, D. C."] <- "Bogotá D.C."
# departamentos$depto[departamentos$depto == "Archipiélago De San Andrés, Providencia Y Santa Catalina"] <- "Archipiélago de San Andrés Providencia y Santa Catalina"
# departamentos$depto[departamentos$depto == "Norte De Santander"] <- "Norte de Santander"
# departamentos$depto[departamentos$depto == "Valle Del Cauca"] <- "Valle del Cauca"

# faltan los distritos especiales... los datos del INS aceptan distritos especiales en vez de departamentos
setdiff(datos$`Departamento o Distrito`, municipios$depto)
#la tabla de departamentos del INS tiene en una categoría aparte cinco ciudades
# pero estas ciudades quedan en departamentos específicos, vamos a crear una
# columna depto para que sea solo el departamento y mantenemos la original con esa distinción

# no se van a poder mapear en paz - pero para tablas y figuras es importante
# separar esas ciudades del resto del departamento
# distritos_especiales <- setdiff(datos$`Departamento o Distrito`, mpos_shp$depto)
# setdiff(mpos_shp$depto, datos$`Departamento o Distrito`)
datos <- datos %>%
   mutate(depto =
            case_when(
              !`Departamento o Distrito` %in% distritos_especiales ~ `Departamento o Distrito`,
              `Departamento o Distrito` == "Cartagena D.T. y C." ~ "Bolívar",
              `Departamento o Distrito` == "Barranquilla D.E." ~ "Atlántico",
              `Departamento o Distrito` == "Santa Marta D.T. y C." ~ "Magdalena",
              `Departamento o Distrito` == "Buenaventura D.E." ~ "Valle del Cauca"))
# count(datos, depto, `Departamento o Distrito`) %>% View()
# toca salvar ambos corregidos
write_csv(datos, "./data/Casos_positivos_de_COVID-19_en_Colombia_corregido.csv")

#toca tranformar municipios
#municipios %>% st_as_sf() %>% st_write("./data/municipios.shp", delete_layer = TRUE)
#departamentos %>% st_as_sf() %>% st_write("./data/departamentos.shp", delete_layer = TRUE)
mpos_shp %>% st_write("./data/municipios.shp", delete_layer = TRUE)
#departamentos %>% st_as_sf() %>% st_write("./data/departamentos.shp", delete_layer = TRUE)

