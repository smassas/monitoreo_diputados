library(rvest) 
library(tidyverse)
library(janitor) 
library(tidyr) 
library(lubridate)
library(stringr)
library(readr)
library(tidytext)
library(syuzhet) 

# Expresiones regulares ---------------------------------

# Una manera de describir patrones específicos de caracteres en un texto.

# \d cualquier dígito
# \w cualquier caracter de palabra
# \s espacio en blanco
# \b límite de palabra
# \D cualquier no dígito
# \W cualquier cosa que no sea caracter de palabra
# \S cualquier cosa que no sean espacios en blanco.
# . cualquier caracter, excepto un salto de línea.

# Posición del patrón dentro de la cadena

# ^: Principio de la cadena,
# $: Final de la cadena.

# Cuantificadores:

# *: El carácter puede aparecer al menos 0 veces.
# +: El carácter puede aparecer al menos 1 vez.
# ?: El carácter puede aparecer hasta 1 vez.

# Otra notación

# [:digit:] o \d. Dígitos del 0 al 9, equivalente a [0-9].
# \D: Inverso de \d, es decir, equivalente a [^0-9].
# [:lower:]: Minúsculas, equivalente a [a-z].
# [:upper:]: Mayúsculas, equivalente a [A-Z]
# [:alpha:]: Mayúsculas y minúsculas.
# [:alnum:]: Caracteres alfanuméricos. Dígitos, mayúsculas y minúsculas. Equivalente a \w.
# \W: Inverso de w.
# [:blank:]: Espacio y tabulación.
# [:space:]: Espacio: tabulación, nueva línea, retorno de línea y espacio.
# [:punct:]: Puntuación.

# Importar datos audiencias  ----------------------------------------------------------

robotstxt::paths_allowed("https://www.camara.cl/transparencia/listadodeaudiencias.aspx")
robotstxt::get_robotstxt("https://www.camara.cl/transparencia/listadodeaudiencias.aspx")

audiencia_diputados <- rvest::read_html("https://www.camara.cl/transparencia/listadodeaudiencias.aspx")

audiencia_diputados <- audiencia_diputados %>% 
                       rvest::html_nodes("table") %>% 
                       rvest::html_table(trim = TRUE)

audiencia_diputados <- tibble(audiencia_diputados[[1]])
audiencia_diputados <- audiencia_diputados %>% janitor::clean_names()

dplyr::glimpse(audiencia_diputados)

# Datos faltantes ---------------------------------------------------------

which(is.na(audiencia_diputados$fecha))
purrr::map_dbl(audiencia_diputados, .f = function(x){sum(is.na(x))})

# Manipulación de fechas --------------------------------------------------

x <- audiencia_diputados$fecha

Sys.getlocale("LC_TIME") 
Sys.setlocale("LC_TIME", "es_ES")

x
class(x)

# Veamos algunas expresiones regulares
stringr::str_view(string = audiencia_diputados$fecha, pattern = "\\.\\s") 

lubridate::guess_formats(x, orders = "dmy", local = "C")

# Veamos si transformamos a formato, día, mes y año.
lubridate::dmy(x)

# sum(is.na(lubridate::dmy(audiencia_diputados$fecha)))
# sum(is.na(audiencia_diputados$fecha))
# as.Date(df$Datum, format="%d.%m.%Y")

# Limpieza de fecha y expresiones regulares------------------------------------------------------------------

audiencia_diputados <- audiencia_diputados %>% 
  dplyr::mutate(fecha = stringr::str_remove_all(fecha, pattern = "\\."),
                fecha = lubridate::dmy(fecha)) %>% 
  dplyr::mutate(across(where(is.character), stringr::str_to_upper),
                sujeto_pasivo = stringr::str_to_title(sujeto_pasivo),
                lobbista_representado = stringr::str_replace_all(lobbista_representado, pattern = "[:blank:]+", " "),
                lugar = stringr::str_replace_all(lugar, pattern = "[:blank:]+", " "),
                materia = stringr::str_replace_all(materia, pattern = "[:blank:]+", " ")) 

audiencia_diputados

# Tablas  ---------------------------------------------------

## Audiencias diputados 2021 ----------------------------------------------------

audiencia_diputados %>% 
  dplyr::select(fecha, everything()) %>% 
  dplyr::group_by(sujeto_pasivo) %>% 
  dplyr::filter(fecha >= "2021-01-01") %>% 
  dplyr::summarise(audiencias = n()) %>% 
  dplyr::arrange(desc(audiencias)) 

## Frecuencia de audiencias según lobbista representado 2021 --------------------------------------

audiencia_diputados %>% 
  dplyr::select(-c(lugar)) %>% 
  dplyr::filter(fecha >= "2021-01-01") %>% 
  dplyr::mutate(lobbista_representado = stringr::str_to_title(lobbista_representado),
                materia = stringr::str_to_sentence(materia)) %>% 
  dplyr::group_by(lobbista_representado) %>% 
  dplyr::count(sujeto_pasivo) %>% 
  dplyr::arrange(desc(n)) 

## Diputados que han tenido más audiencias con lobbista X ----------------------------------------

audiencia_diputados %>% 
  dplyr::mutate(lobbista_representado = stringr::str_to_title(lobbista_representado),
                materia = stringr::str_to_sentence(materia)) %>% 
  dplyr::filter(stringr::str_detect(lobbista_representado, "Walmart")) %>%
  dplyr::count(sujeto_pasivo, sort = TRUE) 

# Veamos el caso de AFP cuprum

audiencia_diputados %>% 
  dplyr::mutate(lobbista_representado = stringr::str_to_title(lobbista_representado),
                materia = stringr::str_to_sentence(materia)) %>% 
  dplyr::filter(stringr::str_detect(lobbista_representado, "Afp Cuprum|Afp Cuprum S.a.")) 

## Audiencia en torno a temas ----------------------------------------------

audiencia_diputados %>% 
  dplyr::select(-lugar) %>% 
  dplyr::mutate(lobbista_representado = stringr::str_to_title(lobbista_representado),
                materia = str_remove(materia, pattern = "TEMA :|TEMA:"),
                materia = stringr::str_to_sentence(materia)) %>% 
  dplyr::filter(stringr::str_detect(materia, "cabify|uber|taxi"))

## Con quién se ha juntado sujeto pasivo ------------------------------------

audiencia_diputados %>% 
  dplyr::select(-lugar) %>% 
  dplyr::mutate(lobbista_representado = stringr::str_to_title(lobbista_representado),
                materia = str_remove(materia, pattern = "TEMA :|TEMA:"),
                materia = stringr::str_to_sentence(materia)) %>% 
  dplyr::group_by(lobbista_representado) %>% 
  dplyr::count(sujeto_pasivo) %>% 
  dplyr::arrange(desc(n))

# Deberíamos filtrar los espacios vacíos y limpiar ciertas expresiones regulares para ver
# cuántos lobbistas no están con registros y sin registros. Para ello, filtremos por un patrón.

aud_sin_registro <- audiencia_diputados %>% 
  dplyr::filter(str_detect(lobbista_representado, pattern = "^\\s*$")) %>% #0 o más espacios en blanco en un string.
  dplyr::mutate(lobbista_representado = ifelse(grepl("^\\s*$", lobbista_representado), 
                                               "No hay registro", lobbista_representado)) 

aud_sin_registro %>% 
  dplyr::group_by(lobbista_representado) %>% 
  dplyr::summarise(Frecuencia = n(),
                   Prop = Frecuencia/nrow(audiencia_diputados))

## Partidos político con más audiencias solicitadas 2021 -------------------------

pie_chart <- audiencia_diputados %>% 
  dplyr::select(sujeto_pasivo, partido, distrito, fecha) %>% 
  dplyr::filter(fecha >= "2021-01-01") %>% 
  dplyr::count(partido) %>% 
  dplyr::mutate(ranking = min_rank(desc(n)),
                porcentaje = round(n/sum(n)*100, 2)) %>% 
  dplyr::arrange(ranking)

library(highcharter)

  highcharter::hchart(pie_chart, "pie", hcaes(name = partido, y = porcentaje),
                    name = "Audiencias",
                    innerSize = "30%") %>% 
  highcharter::hc_chart(style = list(fontFamily = "Oswald")) %>% 
  highcharter::hc_exporting(enabled = TRUE) %>% 
  highcharter::hc_tooltip(borderwidth = 0,
                          backgroundColor = "white") %>% 
  highcharter::hc_add_theme(hc_theme_economist()) %>% 
  highcharter::hc_title(useHTML = TRUE, text = "Audiencias en Cámara de Diputados") %>% 
  highcharter::hc_subtitle(text = "<b>Descripción</b>: Distribución de partidos políticos con más audiencias durante el año 2021") 


  highcharter::hchart(pie_chart, "item", hcaes(name = partido, y = n),
    name = "Audiencias", marker = list(symbol = "square"),
    showInLegend = TRUE) %>% 
    highcharter::hc_chart(style = list(fontFamily = "Oswald")) %>% 
    highcharter::hc_exporting(enabled = TRUE) %>% 
    highcharter::hc_add_theme(hc_theme_538()) %>% 
    highcharter::hc_title(useHTML = TRUE, text = "Audiencias en Cámara de Diputados") %>% 
    highcharter::hc_subtitle(text = "<b>Descripción</b>: Distribución de partidos políticos con más audiencias durante el año 2021") 
  
## Temas que se tratan en audiencias ---------------------------------------

temas_frecuencias <- dplyr::tibble(audiencia_diputados) %>% 
                     dplyr::select(-lugar) %>% 
                     tidytext::unnest_tokens(output = temas_tratados,
                                              input = materia,
                                              strip_numeric = TRUE) %>% 
                     dplyr::count(temas_tratados, sort = TRUE)

stopwords <- readr::read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

stopwords_ad <- dplyr::tibble(palabra = c("chilenos",
                                                 "chile",
                                                 "chilenas",
                                                 "mil",
                                                 "miles",
                                                 "millones"))

temas_frecuencias <- temas_frecuencias %>% 
                     dplyr::anti_join(stopwords, by = c("temas_tratados" = "palabra")) 
                     dplyr::top_n(15) 
                     
View(temas_frecuencias)
                     
# Gráficos ----------------------------------------------------------------

wordcloud2::wordcloud2(temas_frecuencias, fontFamily = "arial")

graf_frecuencias <- temas_frecuencias %>% 
                     dplyr::anti_join(stopwords, by = c("temas_tratados" = "palabra")) %>% 
                     dplyr::top_n(15) 

ggplot2::ggplot(graf_frecuencias, aes(y = reorder(temas_tratados, n), x = n)) + 
  geom_bar(stat = "identity")

# Importar datos afiliación partidaria ------------------------------------

afiliacion <- rvest::read_html("https://www.camara.cl/diputados/diputados.aspx")

nombre <- afiliacion %>% rvest::html_nodes("article h4") %>% html_text()
partido <- afiliacion %>% rvest::html_nodes("article p") %>% html_text()

partido <- as.data.frame(partido)

# Filtremos el distrito
distrito <- partido %>% dplyr::filter(str_detect(partido, pattern = "Distrito"))

# Filtremos el partido
partido <- partido %>% dplyr::filter(str_detect(partido, pattern = "Partido"))

# Unamos tanto el partido como el distrito.
partido <- distrito %>% dplyr::bind_cols(partido) 

# Como tienen un nombre extraño, lo cambiaremos nuevamente por distrito y partido.
partido <- partido %>% dplyr::rename(distrito = partido...1, partido = partido...2) 

# Si nos fijamos bien, la 2da columna tiene acrónimos de los partidos y 3 observaciones
# con el nombre completo. Por ello, cambiaremos eso.

diputados <- partido %>% 
  dplyr::mutate(distrito = stringr::str_remove_all(distrito, "[Distrito: N°|Distrito: Nº]"),
                partido = stringr::str_remove_all(partido, "Partido: "),
                partido = dplyr::case_when(partido == "Renovación Nacional" ~ "RN",
                                           partido == " Evolución Política" ~ "EVOP",
                                           partido == "Partido Por la Democracia" ~ "PPD",
                                           partido == "LIBERAL" ~ "LB",
                                           partido == "COMUNES" ~ "COM",
                                           TRUE ~ partido)) 

# Como ya hicimos los cambios, ahora pegaremos los nombres de diputados/as

diputados <- diputados %>%  
  dplyr::bind_cols(nombre) %>% 
  dplyr::mutate(nombre = stringr::str_replace_all(string = nombre, pattern = "Sr. |Sra. ", replacement = ""),
                nombre = stringr::str_trim(nombre)) %>% 
  dplyr::select(-3) 

# Eliminar lo que ya no utilizaremos.
rm(distrito, partido, nombre)

# Veamos cómo están los nombres en ambas bases.
diputados[diputados$nombre == "Maite Orsini",] # Base 1
audiencia_diputados[audiencia_diputados$sujeto_pasivo == "Maite Orsini Pascal",] # Base 2

# stringr::str_detect(string = audiencia_diputados$sujeto_pasivo, pattern = "Maite")
# stringr::str_locate(string = audiencia_diputados$sujeto_pasivo, pattern = "Maite")

# En base 1 dice Maite Orsini y en base 2 Maite Orsini Pascal. ¿Solución?
stringr::str_split(string = audiencia_diputados$sujeto_pasivo, pattern = boundary("word"), 
                   simplify = TRUE)[,1:3]

# Dividir nombres y apellidos en más columnas. Ya que hay varios que tienen dos a 3 
# apellidos.

diputados_split <- diputados %>% 
  dplyr::mutate(matriz = stringr::str_split(nombre, pattern = boundary("word"), 
                                            simplify = TRUE)) 

diputados_split <- as.data.frame(diputados_split$matriz[,1:3])


# Unamos las columnas para juntar solo el nombre y primer apellido
x <- tidyr::unite(data = diputados_split, col = "nombre", c(V1, V2, V3), sep = " ")


y <- audiencia_diputados %>% 
  dplyr::mutate(y = stringr::str_split(sujeto_pasivo, pattern = boundary("word"), 
                                       simplify = TRUE)) 

y <- as.data.frame(y$y[,1:2])
y <- tidyr::unite(data = y, col = "nombre", c("V1", "V2"), sep = " ")


# Inner join entre audiencias y diputados.
audiencia_diputados <- audiencia_diputados %>% 
  dplyr::mutate(Nombre = stringr::str_split(sujeto_pasivo, boundary("word"),
                                            simplify = TRUE)[,1],
                Apellido = stringr::str_split(sujeto_pasivo, boundary("word"),
                                              simplify = TRUE)[,2]) %>% 
  dplyr::inner_join(y = diputados %>% 
                      dplyr::mutate(Nombre = stringr::str_split(nombre, boundary("word"),
                                                                simplify = TRUE)[,1],
                                    Apellido = stringr::str_split(nombre, boundary("word"),
                                                                  simplify = TRUE)[,2]),
                    by = c("Nombre", "Apellido")) %>% 
  dplyr::select(-c(Nombre, Apellido, nombre))


# Audiencias históricas por diputado ---------------------------

prueba <- rvest::read_html("https://www.camara.cl/transparencia/audiencias.aspx")

# Rescatar nombre diputado
prueba %>% 
  html_nodes(xpath= "//option[1]") %>% 
  html_text()

# Rescatar materias de audiencias por diputado
materias <- prueba %>% 
  rvest::html_nodes(".proyecto") %>% 
  rvest::html_text(trim = TRUE) 

materias <- as.tibble(materias)

# Extraer fechas

str_extract_all(materias$value, '[0-9]{2}/[0-9]{2}/[0-9]{4}')
#str_extract_all(materias$value, "([0]?[1-9]|[1|2][0-9]|[3][0|1])[./-]([0]?[1-9]|[1][0-2])[./-]([0-9]{4}|[0-9]{2})")
#str_extract_all(materias$value, "\\d{1,2}/\\d{1,2}/\\d{1,4}")

fechas <- stringr::str_extract_all(materias$value, '[0-9]{2}/[0-9]{2}/[0-9]{4}')
fechas <- unlist(fechas)

# Extraer duracion

str_extract_all(materias$value, "Duración : \\d{1,2}\\s*\\min.")
duracion <- str_extract_all(materias$value, "Duración : \\d{1,2}\\s*\\min.")
duracion <- unlist(duracion)
  
# Extraer materia

str_extract_all(materias$value, "Materia:")

materias %>% 
  mutate(value = stringr::str_remove_all(value, "\r\n")) 

# Opción alternativa

# materias <- as.data.frame(t(materias)) 
# str_split(string = materias$value, boundary("word"))

materias_2 <- map(materias$value, function(x) {
  tibble(audiencia = unlist(str_split(x, pattern = c("\\n")))) 
  tibble(audiencia = unlist(str_split(x, pattern = c("\r")))) 
})

for(i in 1:length(materias_2)) {       
  materias_2[[i]] <- as.tibble(t(unlist(materias_2[[i]])))[,-2]         
}


audiencias <- bind_rows(materias_2, .id = "audiencia")

# Otras opciones

# prueba <- materias_2 %>% 
#   unlist(recursive = FALSE) %>% 
#   enframe() %>% 
#   unnest()

# do.call("rbind", lapply(materias_2, as.data.frame)) 
# plyr::ldply(materias_2, data.frame)
# do.call(rbind.data.frame, materias_2)


# Función transversal
# materias2 <- map(materias_2, function(x) {as.tibble(t(unlist(materias_2[[i]][["audiencia"]])))[,-2]})

audiencias %>% 
  dplyr::rename(fecha = audiencia1,
                sujeto_pasivo = audiencia3,
                lugar = audiencia4,
                materia = audiencia5,
                decisiones = audiencia6) %>% 
  dplyr::mutate(materia = str_remove_all(materia, "Materia:"),
                decisiones = str_remove_all(decisiones, "Decisiones:"),
                sujeto_pasivo = str_remove_all(sujeto_pasivo, "Duración : \\d{1,2}\\s*\\min."),
                sujeto_pasivo = str_remove_all(sujeto_pasivo, "[^[:alnum:][:blank:]*]")) %>% 
  dplyr::mutate_if(is.character, trimws) 

# gsub(x = materias_2$audiencia3, pattern = "[^[:alnum:]]", replacement = "")
# gsub(x = materias_2$audiencia3, pattern = "[[:punct:]]", replacement = "")
# gsub(x = materias_2$audiencia3, pattern = "[^[:alnum:][:blank:]+?&/\\-]", replacement = "")




