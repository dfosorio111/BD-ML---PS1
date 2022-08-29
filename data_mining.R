#Diego Osorio, Samuel Malkún y Daniel Franco.

## llamar la librería pacman: contiene la función p_load()
rm(list=ls())
require(pacman)
p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest, data.table, dplyr, skimr, # summary data
       caret, rio, vtable, stargazer, ggplot2, boot, MLmetrics) # web-scraping

set.seed(1000)

#Instalar paquetes
install.packages("data,table")
#Cargar librerías
library(data.table)

####Descargar los chunks####

#Creamos una lista que contiene todas las urls de cada dtachunk
lista_urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",1:10,".html")

#Se crea un dat fram vacío
df <- data.frame()
#El ciclo crea un df para cada data_chunk y luego une todos los data frames
for (url in lista_urls) {
  print(url)
  temporal <- read_html(url)%>%html_table()
  df_temportal <- as.data.frame(temporal[[1]])
  df <- rbind(df, df_temportal)
}

#Establecer directorio de trabajo
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning")



#####Data mining and data cleaning####
df2 <- df%>%subset(ocu == 1 & age >=18)


#Url de la complementaria 1
browseURL("https://lectures-r.gitlab.io/big-data-202202/week-01/")
#Guardar las bases de datos en csv
write.csv(df2, "datafiltrada.csv")
write.csv(df, "dataoriginal.csv")

####Estadísticas descriptivas####

## summary db
names(df2)[names(df2) == ''] <- 'Índice'
#Se guarda como tibble
db <- as_tibble(df2)
#Skim
skim(db)%>%head()


head(db$sex, 100)

#Tabla de estadísticas descriptivas con stargazer
stargazer(df2[c("ingtot", "age")], type = "html", title = "Estadísticas Descriptivas", out = "estdec.html")