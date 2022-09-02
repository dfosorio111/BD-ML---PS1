#Diego Osorio, Samuel Malkún y Daniel Franco.


#Set de preguntas
######Punto 2
#Log o no, si utilizamos log borrar NA o qué hacer?
#La variable inglabo suena bien o debería ser ingreso total, podemos usar ambas? ( hay nuevas variables y_income)
#¿Cómo se hace la discusión del model´s in sample fit? ¿Es el R^2, F, MSE, F_1?
#Es un plot de los coeficientes? o qué plot es el que se está pidiendo?
#Ya tenemos el peak age, pero cómo se hace el intervalo de confianza? ¿También quieren los intervalos de los betas?¿Asumimos dist normal?

####Punto 3
#acá es obligatorio logaritmo? o se compara con y sin?
#Misma duda que el 2 con el plot y los intervalos



# 1. importar librarias y set environment/wd
# llamar la librería pacman: contiene la función p_load()
rm(list=ls())

#Establecer directorio de trabajo
#Directorio de Samu
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS1")
#Directorio de Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS1")

#Directorio de Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS1")



set.seed(1000)
require(pacman)
p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest, data.table, dplyr, skimr, # summary data
       caret, rio, vtable, stargazer, ggplot2, boot, MLmetrics, lfe) # web-scraping


#Instalar paquetes
install.packages("data,table")
#Cargar librerías
library(data.table)


# 2. web-scrapping: descargar URL con databases y construir bases

####Descargar los chunks####
# cargar data-set
# creamos una lista que contiene todas las urls de cada datachunk
lista_urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",1:10,".html")

# crear dataframe vacio - estructura de datos
df <- data.frame()
# iterar sobre la lista de urls
for (url in lista_urls) {
  
  # print url actual que contiene el data chunk
  print(url)
  # read_html(): leer url del data-chunk
  # html_table():  crear tabla html
  temporal <- read_html(url)%>%html_table()
  
  # as.data.frame() crea un data-frame temporal
  df_temportal <- as.data.frame(temporal[[1]])
  # rbind(df1,df2): unir el df general con el df temporal
  df <- rbind(df, df_temportal)
}



# 3. Data mining and data cleaning

# %>% - (df.fun1.fun2 in Python)
# df%>%subset: overwrite sobre df on condition ocu == 1 & age >=18

df2 <- df%>%subset(ocu == 1 & age >=18)


#Url de la complementaria 1
browseURL("https://lectures-r.gitlab.io/big-data-202202/week-01/")

# guardar/escribirlas bases de datos en csv en el wd establecido
write.csv(df2, "datafiltrada.csv")
write.csv(df, "dataoriginal.csv")


# read_csv(file):  cargar database
df <- read.csv("dataoriginal.csv")
df2 <- read.csv("datafiltrada.csv")



# 4. Estadísticas descriptivas: análisis estadístico y descripción de variables


## summary db
names(df2)[names(df2) == ''] <- 'Índice'
# Se guarda como tibble
db <- as_tibble(df2)
# skim
skim(db)%>%head()
head(db$sex, 100)


#Tabla de estadísticas descriptivas con stargazer
stargazer(df2[c("ingtot", "age")], type = "html", title = "Estadísticas Descriptivas", out = "estdec.html")


#Gráficos#

# boxplot estrato vs ingreso total g1
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




# scatter sin puntos de edad vs ingreso total g2
ggplot(df2, aes(x = age, y = log(ingtot)))+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


# scatter edad vs impa g3
ggplot(df2, aes(x = age, y = log(impa)))+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Ingreso monetario de primera actividad según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso monetario de primera actividad")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


# scatter con puntos de edad vs ingreso total b4
ggplot(df2, aes(x = age, y = log(ingtot)))+
  geom_point()+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


#Scatter del y_laboral vs edad
ggplot(df2, aes(x = age, y = log(y_ingLab_m)))+
  geom_point()+
  geom_smooth(method = "loess", level = 0.95, aes(weight = fex_c))+
  ggtitle("Perfil de ingreso laboral y edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso laboral mensual")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


# 5. Modelos de Regresion Lineal

#relación ingreso y edad
# mutate(new_col=f(var)): crea nueva columna a partir de existentes
# mutate(x=var):permite crear nuevas variables a partir de otras variables para construir funciones f(y), f(x)
df2 <- df2%>%mutate(age2=age^2)
df2 <- df2%>%mutate(inglabo=impa+isa)


### Bootstraping ###
# boot(database, eta_fun, R=N):  permite obtener los estimadores t(beta) del modelo



summary(lm(y_ingLab_m~age+age2, data = df2, weights = fex_c))




eta.fn_1<-function(data,index){
  coef(lm(y_ingLab_m~age+age2, data = df2, weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
boot1 <- boot(df2, eta.fn_1, R = 1000)
output_boot1 <- t(rbind(boot1$t0, apply(boot1$t, 2, function(x) sd(x))))  



eta.fn_11<-function(data,index){
  
  f<-lm(y_ingLab_m~age+age2, data = df2, weights = fex_c, subset = index)
  coefs_11 <- f$coefficients
  b2<-coefs_11[2]
  b3<-coefs_11[3]
  age_peak_nolog<- -b2/(2*b3)
  return(age_peak_nolog)
  
}

# boot(data, eta_func, R=N)
boot11 <- boot(df2, eta.fn_11, R = 1000)

output_boot11 <- t(rbind(boot11$t0, apply(boot11$t, 2, function(x) sd(x))))  



alpha <- 0.05
age_peak_min_nolog <- output_boot11[1,1]-qnorm(1-(alpha/2))*output_boot11[1,2]
age_peak_max_nolog <- output_boot11[1,1]+qnorm(1-(alpha/2))*output_boot11[1,2]
#Guardar los betas
b1_1 <- boot1$t0[1]
b2_1 <- boot1$t0[2]
b3_1 <- boot1$t0[3]

#Betas máximos
b1_1_max <- boot1$t0[1]+qnorm(1-(alpha/2))*output_boot1[1,2]
b2_1_max <- boot1$t0[2]+qnorm(1-(alpha/2))*output_boot1[2,2]
b3_1_max <- boot1$t0[3]+qnorm(1-(alpha/2))*output_boot1[3,2]

#Betas mínimos
b1_1_min <- boot1$t0[1]-qnorm(1-(alpha/2))*output_boot1[1,2]
b2_1_min <- boot1$t0[2]-qnorm(1-(alpha/2))*output_boot1[2,2]
b3_1_min <- boot1$t0[3]-qnorm(1-(alpha/2))*output_boot1[3,2]


df2 <- df2%>%mutate(inglabo_hat_nolog = b1_1+b2_1*age+b3_1*age2)
df2 <- df2%>%mutate(inglabo_hat_nolog_max = b1_1_max+b2_1_max*age+b3_1_max*age2)
df2 <- df2%>%mutate(inglabo_hat_nolog_min = b1_1_min+b2_1_min*age+b3_1_min*age2)



#mse_1 <- df2%>%subset(is.na(y_ingLab_m)==FALSE)%>%mutate(mse1 =(y_ingLab_m-inglabo_hat_nolog)^2)%>%summarise(rmse=(mean(mse1)) )



df2 <- df2%>%group_by(directorio)%>%mutate(mean_y_ingLab_m= mean(y_ingLab_m, na.rm=TRUE))
df2 %>%select(directorio, y_ingLab_m, mean_y_ingLab_m)%>%tail()
df2$y_ingLab_m_def <- df2$y_ingLab_m
df2$y_ingLab_m_def[which(is.na(df2$y_ingLab_m_def)==TRUE)] <- df2$mean_y_ingLab_m[which(is.na(df2$y_ingLab_m_def)==TRUE)]
df2 %>%select(directorio, y_ingLab_m, mean_y_ingLab_m, y_ingLab_m_def)%>%tail()



df2mse <- df2%>%subset(is.na(y_ingLab_m_def)==FALSE)%>%mutate(mse1=(y_ingLab_m_def-inglabo_hat_nolog)^2)
View(df2mse %>%select(directorio, y_ingLab_m, mean_y_ingLab_m, y_ingLab_m_def,mse1, inglabo_hat_nolog)%>%head(30))
mean(df2mse$mse1)
sqrt(mean(df2mse$mse1))



summary(lm(y_ingLab_m_def~age+age2, data = df2mse, weights = fex_c))






#Con intervalos de los betas
ggplot(df2)+
  geom_line(mapping = aes(x = age, y = inglabo_hat_nolog_max/1000))+
  geom_line(mapping = aes(x = age, y = inglabo_hat_nolog_min/1000))+
  geom_line(mapping = aes(x = age, y = inglabo_hat_nolog/1000))+
  ggtitle("Perfil ingreso laboral y edad")+
  xlab("Edad")+
  ylab("Ingreso laboral estimado en miles")+
  theme_classic()+
  geom_vline(xintercept = age_peak_min_nolog, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = age_peak_max_nolog, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_boot11[1,1], linetype="dotted", color = "blue", size=1)+
  geom_text(aes(x=48, label="\nEdad pico=48.5 ", y=1000), colour="blue", angle=90)+
  geom_text(aes(x=45.8, label="\nRango minimo=48.5", y=1000), colour="red", angle=90)+
  geom_text(aes(x=51.2, label="\nRango máximo=51.2", y=1000), colour="red", angle=90)+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )



#Sin intervalos de los betas
ggplot(df2)+
  geom_line(mapping = aes(x = age, y = inglabo_hat_nolog/1000))+
  ggtitle("Perfil ingreso laboral y edad")+
  xlab("Edad")+
  ylab("Ingreso laboral estimado en miles")+
  theme_classic()+
  geom_vline(xintercept = age_peak_min_nolog, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = age_peak_max_nolog, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_boot11[1,1], linetype="dotted", color = "blue", size=1)+
  geom_text(aes(x=48, label="\nEdad pico=48.5 ", y=1000), colour="blue", angle=90)+
  geom_text(aes(x=45.8, label="\nRango minimo=45.8", y=1000), colour="red", angle=90)+
  geom_text(aes(x=51.2, label="\nRango máximo=51.2", y=1000), colour="red", angle=90)+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )





eta.fn_2<-function(data,index){
  coef(lm(log(y_ingLab_m)~age+age2, data = df2, weights = fex_c, subset = index))
}

boot2 <- boot(df2, eta.fn_2, R = 1000)
output_boot2 <- t(rbind(boot2$t0, apply(boot2$t, 2, function(x) sd(x))))  

eta.fn_22<-function(data,index){
  
  f<-lm(log(y_ingLab_m)~age+age2, data = df2, weights = fex_c, subset = index)
  coefs_22 <- f$coefficients
  b2<-coefs_22[2]
  b3<-coefs_22[3]
  age_peak_log<- -b2/(2*b3)
  return(age_peak_log)
  
}

# boot(data, eta_func, R=N)
boot22 <- boot(df2, eta.fn_22, R = 1000)

output_boot22 <- t(rbind(boot22$t0, apply(boot22$t, 2, function(x) sd(x))))  


alpha <- 0.05
age_peak_min_log<- output_boot22[1,1]-qnorm(1-(alpha/2))*output_boot22[1,2]
age_peak_max_log <- output_boot22[1,1]+qnorm(1-(alpha/2))*output_boot22[1,2]
#Guardar los betas
b1_2 <- boot2$t0[1]
b2_2 <- boot2$t0[2]
b3_2 <- boot2$t0[3]

#Betas máximos
b1_2_max <- boot2$t0[1]+qnorm(1-(alpha/2))*output_boot2[1,2]
b2_2_max <- boot2$t0[2]+qnorm(1-(alpha/2))*output_boot2[2,2]
b3_2_max <- boot2$t0[3]+qnorm(1-(alpha/2))*output_boot2[3,2]

#Betas mínimos
b1_2_min <- boot2$t0[1]-qnorm(1-(alpha/2))*output_boot2[1,2]
b2_2_min <- boot2$t0[2]-qnorm(1-(alpha/2))*output_boot2[2,2]
b3_2_min <- boot2$t0[3]-qnorm(1-(alpha/2))*output_boot2[3,2]


df2 <- df2%>%mutate(inglabo_hat_log = exp(b1_2+b2_2*age+b3_2*age2))
df2 <- df2%>%mutate(inglabo_hat_log_max = exp(b1_2_max+b2_2_max*age+b3_2_max*age2))
df2 <- df2%>%mutate(inglabo_hat_log_min = exp(b1_2_min+b2_2_min*age+b3_2_min*age2))



#Models's in sample fit







#Con intervalos de los betas
ggplot(df2)+
  geom_line(mapping = aes(x = age, y = log(inglabo_hat_log_max)))+
  geom_line(mapping = aes(x = age, y = log(inglabo_hat_log_min)))+
  geom_line(mapping = aes(x = age, y = log(inglabo_hat_log)))+
  ggtitle("Perfil ingreso laboral y edad")+
  xlab("Edad")+
  ylab("logaritmo del Ingreso laboral estimado")+
  theme_classic()+
  geom_vline(xintercept = age_peak_min_log, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = age_peak_max_log, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_boot22[1,1], linetype="dotted", color = "blue", size=1)+
  geom_text(aes(x=43.1, label="\nEdad pico=43.1 ", y=12), colour="blue", angle=90)+
  geom_text(aes(x=42.3, label="\nRango minimo=42.3", y=12), colour="red", angle=90)+
  geom_text(aes(x=43.9, label="\nRango máximo=43.9", y=12), colour="red", angle=90)+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )

#Sin intervalos de los betas
ggplot(df2)+
  geom_line(mapping = aes(x = age, y = log(inglabo_hat_log)))+
  ggtitle("Perfil ingreso laboral y edad")+
  xlab("Edad")+
  ylab("logaritmo del Ingreso laboral estimado")+
  theme_classic()+
  geom_vline(xintercept = age_peak_min_log, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = age_peak_max_log, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_boot22[1,1], linetype="dotted", color = "blue", size=1)+
  geom_text(aes(x=43.1, label="\nEdad pico=43.1 ", y=12), colour="blue", angle=90)+
  geom_text(aes(x=42.3, label="\nRango minimo=42.3", y=12), colour="red", angle=90)+
  geom_text(aes(x=43.9, label="\nRango máximo=43.9", y=12), colour="red", angle=90)+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


#Sacar la tabla
stargazer(output_boot1, type = "html", title = "Ingreso laboral", out = "mod1.html")
stargazer(output_boot2, type = "html", title = "Logaritmo del Ingreso laboral", out = "mod2.html")


#Age peak: igualar la función derivada de y = 0  
age_peak1 <- -(boot1$t0[2]/(2*boot1$t0[3]))
age_peaklog <- -(boot2$t0[2]/(2*boot2$t0[3]))


eta.fn_3<-function(data,index){
  coef(lm(inglabo~age+age2, data = df2, weights = fex_c, subset = index))
}

boot3 <- boot(df2, eta.fn_3, R = 1000)

eta.fn_4<-function(data,index){
  coef(lm(log(inglabo+1)~age+age2, data = df2, weights = fex_c, subset = index))
}

boot4 <- boot(df2, eta.fn_4, R = 1000)

# mutate(x=var):permite crear nuevas variables a partir de otras variables para construir funciones f(y), f(x)
# inglabo_hat(fun_predic), inglabo_hat_nolog(fun_predic)
df2 <- df2%>%mutate(inglabo_hat = exp(boot2$t0[1]+boot2$t0[2]*age+boot2$t0[3]*age2))
df2 <- df2%>%mutate(inglabo_hat_nolog = boot1$t0[1]+boot1$t0[2]*age+boot1$t0[3]*age2)

# gráfico solo con valores predichos g5
ggplot(df2, aes(x = age, y = log(inglabo_hat)))+
  geom_line()+
  ggtitle("Perfil ingreso laboral y edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso laboral estimado")+
  theme_classic()+
  geom_vline(xintercept = 43.1, linetype="dotted", color = "red", size=1.5)+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


ggplot(df2, aes(x = age, y = inglabo_hat_nolog/1000))+
  geom_line()+
  ggtitle("Perfil ingreso laboral y edad")+
  xlab("Edad")+
  ylab("Ingreso laboral estimado en miles")+
  theme_classic()+
  ylim(200,2500)+
  geom_vline(xintercept = 48.5, linetype="dotted", color = "red", size=1)+
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



#Age peak: igualar la función derivada de y = 0  
age_peak <- -(boot2$t0[2]/(2*boot2$t0[3]))
age_peak_nolog <- -(boot3$t0[2]/(2*boot3$t0[3]))




#Obtener errores estándar de los estimadores
output_tab <- t(rbind(boot3$t0, apply(boot3$t, 2, function(x) sd(x))))  

# Intervalos de confianza

alpha <- 0.05
age_peak_min_nolog <- -((boot3$t0[2]- qnorm(alpha/2)*output_tab[2,2])/(2*(boot3$t0[3]- qnorm(alpha/2)*output_tab[3,2])))
age_peak_max_nolog <- -((boot3$t0[2]+ qnorm(alpha/2)*output_tab[2,2])/(2*(boot3$t0[3]+ qnorm(alpha/2)*output_tab[3,2])))



####Punto 3####
#Crear la variable female


# mutate(var=dep) 
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


# modelo regresion lineal
# f(y) = log(inglabo), inglabo=impa+isa
# f(x) = female*age+female*age2
# df2%>%subset(inglabo > 0)  overwrite df2 on condition:inglabo > 0
# factor de expansión:  weights = fex_c 

p3m7 <- lm(data = df2%>%subset(inglabo > 0), log(inglabo) ~ female*age+female*age2, weights = fex_c)
summary(p3m7)

p3m8 <- lm(data = df2, inglabo ~ female*age+female*age2, weights = fex_c)
summary(p3m8)


### Bootstraping ###

# bootstrap para p3m7 
eta.fn_p3m7<-function(data,index){
  coef(lm(log(y_ingLab_m) ~ female*age+female*age2, data = df2, weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
bootp3m7 <- boot(df2, eta.fn_p3m7, R = 1000)


eta.fn_p3m1<-function(data,index){
  coef(lm(log(ingtot+1) ~ female, data = df2, weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
bootp3m1 <- boot(df2, eta.fn_p3m1, R = 1000)



eta.fn_p3m2<-function(data,index){
  coef(lm(log(ingtot) ~ female, data = df2%>%subset(ingtot > 0), weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
bootp3m2 <- boot(df2, eta.fn_p3m2, R = 1000)



eta.fn_p3m3<-function(data,index){
  coef(lm(log(ingtot+1) ~ female*age+female*age2, data = df2, weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
bootp3m3 <- boot(df2, eta.fn_p3m3, R = 1000)


eta.fn_p4m4<-function(data,index){
  coef(lm(log(ingtot) ~ female*age+female*age2, data = df2%>%subset(ingtot > 0), weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
bootp4m4 <- boot(df2, eta.fn_p4m4, R = 1000)

eta.fn_p5m5<-function(data,index){
  coef(lm(ingtot ~ female*age+female*age2, data = df2, weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
bootp5m5 <- boot(df2, eta.fn_p5m5, R = 1000)



## graficas: scatter plots

# f(y) = log(inglabo), inglabo=impa+isa
# f(x) = female*age+female*age2
# df2%>%subset(inglabo > 0)  overwrite df2 on condition:inglabo > 0

# crear variables para predictores
# mutate(x=var):permite crear nuevas variables a partir de otras variables para construir funciones f(y), f(x)
# inglabo_hat(fun_predic), inglabo_hat_nolog(fun_predic)


# inglaboFem_hat_m: funcion prediccion de inglaboFem
df2 <- df2%>%mutate(inglaboFem_hat_m = exp(bootp3m7$t0[1]+bootp3m7$t0[2]*female+bootp3m7$t0[3]*age+bootp3m7$t0[4]*age2+bootp3m7$t0[5]*female*age+bootp3m7$t0[6]*female*age2))



# gráfico solo con valores predichos g5
ggplot(df2, aes(x = age, y = log(inglaboFem_hat_m), group=as.factor(female), color=as.factor(female))  ) +
  geom_point()+
  ggtitle("Ingreso laboral según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )




# gráfico solo con valores originales g6
ggplot(df2%>%subset(inglabo>0), aes(x = age, y = log(inglabo), group=as.factor(female), color=as.factor(female))  ) +
  geom_point()+
  ggtitle("Ingreso total según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )






#Gráfico con ambos g6
ggplot(data = df2%>%subset(inglabo>0))+
  geom_point(mapping = aes(x = age, y = log(inglabo)), color = "blue")+
  geom_point(mapping = aes(x = age, y = log(inglaboFem_hat_m)), color = "red")+
  #geom_point(mapping = aes(x = age, y = log(inglabo_hat_nolog)), color = "orange")+
  ggtitle("Ingreso total según la edad")+
  xlab("Edad")+
  ylab("Logaritmo del ingreso total")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14) )


#Age peak: igualar la función derivada de y = 0 

# caso hombres
age_peak_hom <- -(bootp3m7$t0[3])/(2*bootp3m7$t0[4])


# caso mujeres
age_peak_fem <- -(bootp3m7$t0[3]+bootp3m7$t0[5])/(2*(bootp3m7$t0[4]+bootp3m7$t0[6]))



### FALTA DETERMINAR LA FUNCION PARA CALCULAR LOS INTERVALOS DE CONFIANZA
#Sacar los intervalos para el peak age con sus IC
#Obtener errores estándar de los estimadores****

output_tab <- t(rbind(boot3$t0, apply(boot3$t, 2, function(x) sd(x))))  

# Intervalos de confianza

alpha <- 0.05
age_peak_min_nolog <- -((boot3$t0[2]- qnorm(alpha/2)*output_tab[2,2])/(2*(boot3$t0[3]- qnorm(alpha/2)*output_tab[3,2])))
age_peak_max_nolog <- -((boot3$t0[2]+ qnorm(alpha/2)*output_tab[2,2])/(2*(boot3$t0[3]+ qnorm(alpha/2)*output_tab[3,2])))



#Punto 3 c
names(df2)

# contruir nueva base para gap con variables de control
dfbase<- df2[c("age", "age2", "female", "clase", "p6210", "relab","p6210s1", "college", "cotPension", "cuentaPropia", "estrato1", "fex_c", "formal", "fweight", "hoursWorkUsual", "inglabo", "ingtot", "impa", "isa", "maxEducLevel", "mes", "microEmpresa", "oficio", "p6050","p6426", "y_ingLab_m", "y_ingLab_m_ha", "y_salarySec_m", "y_salary_m_hu", "y_total_m", "y_total_m_ha")]
write.csv(dfbase, "datagap.csv")
dfgap <- read.csv("datagap.csv")



# modelos prueba fwl manual

# modelo lineal - variables de control: maxEducLevel, factor(estrato1)

modelo_gap2 <- lm(data = dfgap, log(y_total_m)~ age*female+age2*female+factor(maxEducLevel) , weights = fex_c)
summary(modelo_gap2)


# modelo fwl (matriz de proyección y annihilation)
modelo_gap4_fwl <- felm(log(y_total_m)~ age*female+age2*female|factor(maxEducLevel), data =dfgap, weights = dfgap$fex_c)
summary(modelo_gap4_fwl)


prueba1 <- lm(data = dfgap, log(y_total_m)~ factor(maxEducLevel)+factor(female))$residuals
summary(prueba1)

prueba2 <- felm(data = dfgap, log(y_total_m)~ factor(female)|factor(maxEducLevel))
summary(prueba2)


# fwl manual

dfgap <- dfgap%>%mutate(res_y = lm(data = dfgap, log(y_total_m)~ factor(maxEducLevel))$residuals,
                        res_x = lm(data = dfgap, female ~ factor(maxEducLevel))$residuals)

prueba3 <- lm(res_y~res_x-1,dfgap)
summary(prueba3)




### LOS MODELOS DE 3 SON CON LOG


# modelo lineal - variables de control: maxEducLevel

modelo_gap1 <- lm(data = dfgap, y_total_m ~ age*female+age2*female+factor(maxEducLevel) , weights = fex_c)
summary(modelo_gap1)

# modelo lineal - variables de control: maxEducLevel, factor(estrato1)
modelo_gap2 <- lm(data = dfgap, log(y_total_m)~ age*female+age2*female+factor(maxEducLevel)+ factor(estrato1) , weights = fex_c)
summary(modelo_gap1)



# modelo lineal - variables de control: maxEducLevel, factor(estrato1)
modelo_gap3 <- lm(data = dfgap, y_total_m ~ age*female+age2*female+factor(maxEducLevel)+ factor(estrato1) , weights = fex_c)
summary(modelo_gap1)



# modelo lineal - variables de control: nivel educativo, estrato1, relab, oficio1
# basado en ncbi:literatura

dfgap2 <- dfgap%>%subset(y_ingLab_m>0)

modelo_gap4 <- lm(data = dfgap2, log(y_ingLab_m) ~ age*female+age2*female+factor(maxEducLevel)+ factor(estrato1) + factor(relab), weights = fex_c)
summary(modelo_gap4)








# modelo fwl (matriz de proyección y annihilation)
modelo_gap4_fwl <- felm(log(y_ingLab_m) ~ age*female+age2*female| factor(maxEducLevel)+ factor(estrato1)+ factor(relab), data =dfgap2, weights = dfgap2$fex_c)
summary(modelo_gap4_fwl)



# bootstping para modelo con fwl (matriz de proyección y annihilation)

eta.fn_gap4<-function(data,index){
  coef(lm(log(y_ingLab_m) ~ age*female+age2*female+factor(maxEducLevel)+ factor(estrato1) + factor(relab), data = dfgap2, weights = fex_c, subset = index))
}



# boot(data, eta_func, R=N)
bootgap4 <- boot(dfgap2, eta.fn_gap4, R = 1000)

eta.fn_gap5<-function(data,index){
  coef(lm(log(y_ingLab_m) ~ age*female+age2*female, data = dfgap2, weights = fex_c, subset = index))
}

bootgap5 <- boot(dfgap2, eta.fn_gap5, R = 1000)

bootgap5

eta.fn_gap6<-function(data,index){
  coef(felm(log(y_ingLab_m) ~ age*female+age2*female| factor(maxEducLevel)+ factor(estrato1)+ factor(relab), data = dfgap2, weights = dfgap2$fex_c, subset = index))
}

bootgap6 <- boot(dfgap2, eta.fn_gap6, R = 1000)

bootgap6

## Punto 4: Prediction and Performance Evaluation
# prediction, overfitting and cross-val

# split database into test-set and test-set

p_load(tidyverse, fabricatr, stargazer)

# set/crear seed para reproducibilidad
set.seed(101010)



# read_csv(file):  cargar database
df_ml <- read.csv("datafiltrada.csv")




# pre-processing
df_ml%>%select(directorio)%>%head(10)

df_ml_1 <- df_ml%>%group_by(directorio)%>%mutate(mean_y_ingLab_m= mean(y_ingLab_m, na.rm=TRUE))
df_ml_1$y_ingLab_m_def <- df_ml_1$y_ingLab_m
df_ml_1$y_ingLab_m_def[which(is.na(df_ml_1$y_ingLab_m_def)==TRUE)] <- df_ml_1$mean_y_ingLab_m[which(is.na(df_ml_1$y_ingLab_m_def)==TRUE)]

df_ml$y_ingLab_m_def <- df_ml_1$y_ingLab_m_def

# crear dataframe para fit/train el modelo

df_ml <- df_ml%>%subset(is.na(y_ingLab_m)==FALSE)

id_train <- sample(1:nrow(df_ml), size=0.7*nrow(df_ml), replace=FALSE)


# split dataset en train-set y test-set para entrenar el modelo y realizar predicciones
test_set <- df_ml[-id_train,]
train_set <- df_ml[id_train,]

# split base en train-set y test-set

y_train <- select(train_set, y_ingLab_m)
x_train <- select(train_set, age, age2, female)

y_test <- select(test_set, y_ingLab_m)
x_test <- select(test_set, age, age2, female)


# fit/train modelo con train-set

# regresion lineal

train_base <-cbind(y_train,x_train)
modelo1 <- lm(y_ingLab_m~age*female+age2*female, data = train_base)
summary(modelo1)


# prediccion
y_predict_train <- predict(modelo1, newdata = x_train)
y_predict_test <- predict(modelo1, newdata = x_test)


# metricas de evaluacion

# Métricas dentro y fuera de muestra. Paquete MLmetrics
r2_train <- R2_Score(y_pred = y_predict_train, y_true = y_train$y_ingLab_m)
rmse_train <- RMSE(y_pred = y_predict_train, y_true = y_train$y_ingLab_m)


r2_test <- R2_Score(y_pred = y_predict_test, y_true = y_test)
rmse_test <- RMSE(y_pred = exp(y_hat_out1), y_true = exp(y_test))

resultados <- data.frame(Modelo = "Regresión lineal", 
                         Muestra = "Dentro",
                         R2_Score = r2_in1, RMSE = rmse_in1) %>%
  rbind(data.frame(Modelo = "Regresión lineal", 
                   Muestra = "Fuera",
                   R2_Score = r2_out1, RMSE = rmse_out1))


plot(lm(y_ingLab_m~age*female+age2*female, data = train_base))






# fit/train ajustar el modelo
spec1 <- lm(y_ingLab_m_def~age+age2, data = train_set)
summary(spec1)


# predict: predecir los valores de nuestro modelo con test-set

test_set$spec1 <- predict(spec1, newdata = test_set)
with(test_set, mean(y_ingLab_m_def-spec1)^2 )











