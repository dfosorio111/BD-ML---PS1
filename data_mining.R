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

#Cargar la base
df2 <- read.csv("datafiltrada.csv")


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

####Gráficos####
#Boxplot estrato vs ingreso total g1
ggplot(df2, aes(x = as.factor(estrato1) , y = log(ingtot) , fill = as.factor(sex))) +
  geom_boxplot()+
  scale_fill_hue(l=60, c=80)+
  ggtitle("Ln del ingreso total según estrato social y sexo")+
  xlab("Estrato o ICV")+
  ylab("Ln del ingreso total")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Sexo")+
  theme_classic()+
  theme(text = element_text(size = 16), plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 14), legend.title = element_text(size = 16))+
  scale_fill_manual(values = c("0" ="red" , "1"="blue"), label = c("0" ="Mujer" , "1"="Hombre"))


#Scatter sin puntos de edad vs ingreso total g2
ggplot(df2, aes(x = age, y = log(ingtot)))+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


#Scatter edad vs impa g3
ggplot(df2, aes(x = age, y = log(impa)))+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Ingreso monetario de primera actividad según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso monetario de primera actividad")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


#Scatter con puntos de edad vs ingreso total b4
ggplot(df2, aes(x = age, y = log(ingtot)))+
  geom_point()+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )




####Regresiones####
#relación ingreso y edad

df2 <- df2%>%mutate(age2=age^2)
df2 <- df2%>%mutate(inglabo=impa+isa)


#Bootstrap
eta.fn_1<-function(data,index){
  coef(lm(impa~age+age2, data = df2, weights = fex_c, subset = index))
}

boot1 <- boot(df2, eta.fn_1, R = 1000)

eta.fn_2<-function(data,index){
  coef(lm(log(impa+1)~age+age2, data = df2, weights = fex_c, subset = index))
}

boot2 <- boot(df2, eta.fn_2, R = 1000)

eta.fn_3<-function(data,index){
  coef(lm(inglabo~age+age2, data = df2, weights = fex_c, subset = index))
}

boot3 <- boot(df2, eta.fn_3, R = 1000)

eta.fn_4<-function(data,index){
  coef(lm(log(inglabo+1)~age+age2, data = df2, weights = fex_c, subset = index))
}

boot4 <- boot(df2, eta.fn_4, R = 1000)

df2 <- df2%>%mutate(inglabo_hat = exp(boot4$t0[1]+boot4$t0[2]*age+boot4$t0[3]*age2)-1)
df2 <- df2%>%mutate(inglabo_hat_nolog = boot3$t0[1]+boot3$t0[2]*age+boot3$t0[3]*age2)

#Gráfico solo con valores predichos g5
ggplot(df2, aes(x = age, y = log(inglabo_hat)))+
  geom_point()+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )

#Gráfico con ambos g6
ggplot(data = df2)+
  geom_point(mapping = aes(x = age, y = log(inglabo)), color = "blue")+
  geom_point(mapping = aes(x = age, y = log(inglabo_hat)), color = "red")+
  geom_point(mapping = aes(x = age, y = log(inglabo_hat_nolog)), color = "orange")+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )

#Gráfico sin logaritmos g7
ggplot(data = df2)+
  geom_point(mapping = aes(x = age, y = (inglabo)), color = "blue")+
  geom_point(mapping = aes(x = age, y = (inglabo_hat)), color = "red")+
  geom_point(mapping = aes(x = age, y = (inglabo_hat_nolog)), color = "orange")+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


#Age peak
age_peak <- -(boot4$t0[2]/(2*boot4$t0[3]))
age_peak_nolog <- -(boot3$t0[2]/(2*boot3$t0[3]))



#Obtener errores estándar de los estimadores
output_tab <- t(rbind(boot3$t0, apply(boot3$t, 2, function(x) sd(x))))  

alpha <- 0.05
age_peak_min_nolog <- -((boot3$t0[2]- qnorm(alpha/2)*output_tab[2,2])/(2*(boot3$t0[3]- qnorm(alpha/2)*output_tab[3,2])))
age_peak_max_nolog <- -((boot3$t0[2]+ qnorm(alpha/2)*output_tab[2,2])/(2*(boot3$t0[3]+ qnorm(alpha/2)*output_tab[3,2])))

####Punto 3####
#Crear la variable female
df2 <- df2%>%mutate(female = 1-sex)

p3m1 <- lm(data = df2, log(ingtot+1) ~ female, weights = fex_c)
summary(p3m1)

p3m2 <- lm(data = df2%>%subset(ingtot > 0), log(ingtot) ~ female, weights = fex_c)
summary(p3m2)


#Age earnings by gender
p3m3 <- lm(data = df2, log(ingtot+1) ~ female*age+female*age2, weights = fex_c)
summary(p3m3)

p3m4 <- lm(data = df2%>%subset(ingtot > 0), log(ingtot) ~ female*age+female*age2, weights = fex_c)
summary(p3m4)

p3m5 <- lm(data = df2, ingtot ~ female*age+female*age2, weights = fex_c)
summary(p3m5)

#Cambiando ingtot por inglabo
p3m6 <- lm(data = df2, log(inglabo+1) ~ female*age+female*age2, weights = fex_c)
summary(p3m6)

p3m7 <- lm(data = df2%>%subset(inglabo > 0), log(inglabo) ~ female*age+female*age2, weights = fex_c)
summary(p3m7)

p3m8 <- lm(data = df2, inglabo ~ female*age+female*age2, weights = fex_c)
summary(p3m8)

#Correr el bootstrap para p3m7 o el que te parezca más acertado
#Intentemos sacar la gráfica del 2 para el mpdelo p3m7 como el g6
#Al hacer el gráfico toca colorear los puntos según el sexo, eso es con 
browseURL("https://ggplot2.tidyverse.org/reference/geom_point.html")
#Sacar los intervalos para el peak age con sus IC


