#Punto 3
#Establecer directorio de trabajo
#Directorio de Samu
setwd("~/Desktop/Big Data/Repositorios/BD-ML---PS1")
#Directorio de Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS1")
#Directorio de Diego
setwd("C:/Users/Diego/OneDrive/Documents/GitHub/BD-ML---PS1")


#Descargar paquetes
set.seed(1000)
require(pacman)
p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest, data.table, dplyr, skimr, # summary data
       caret, rio, vtable, stargazer, ggplot2, boot, MLmetrics, lfe, tidyverse, fabricatr, stargazer,
       Hmisc, writexl) # web-scraping


#Importar la base de datos
df2 <- read.csv("datafiltrada.csv")

#Revisión de NA'S
is.na(df2$y_total_m)%>%table()
is.na(df2$female)%>%table()

#Tenemos 6650 NaN's en la variable de ingreso y se eliminan esas observaciones. 
df2_2 <- df2%>%subset(is.na(y_total_m) == FALSE)
is.na(df2_2$y_total_m)%>%table()
#se crea la variable de logaritmo del ingreso laboral
df2_2 <- df2_2%>%mutate(log_y = log(y_total_m))

#Distribución hombres y mujeres
df2%>%count(female, wt = fex_c)%>%mutate(per = n/sum(n))
df2_2%>%count(female, wt = fex_c)%>%mutate(per = n/sum(n))

#Correr el modelo incondicional (se eliminan todos los NA para no incluir sesgos en los datos)
p3m1 <- lm(data = df2_2, log_y ~ female, weights = fex_c/12)
summary(p3m1)
stargazer(p3m1, type = "html", title = "", out = "p3m1.html")
p3c1 <- p3m1$coefficients
exp(p3c1[1])
exp(p3c1[1]+p3c1[2])

#Estadísticas del salario
weighted.mean(df2_2$y_total_m, w = df2_2$fex_c/12)
weighted.sd(df2_2$y_total_m, w = df2_2$fex_c/12)
wtd.quantile(x = df2_2$y_total_m, weights=df2_2$fex_c/12, probs=seq(0, 1, 0.25), 
             type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
             normwt=FALSE, na.rm=TRUE)

#Incluyendo la edad para ver las brechas salariales a lo largo de la vida
#Se crea edad al cuadrado
df2_2 <- df2_2%>%mutate(age2 = age^2)
#interacción edad y género
df2_2 <- df2_2%>%mutate(age_female = age*female)
df2_2 <- df2_2%>%mutate(age2_female = age2*female)

# bootstrap 
eta.fn_p3m1<-function(data,index){
  coef(lm(log_y ~ female + age + age2  + age_female + age2_female, data = df2_3, weights = fex_c/12, subset = index))
}

# boot(data, eta_func, R=N)
bootp3m1 <- boot(df2_3, eta.fn_p3m1, R = 1000)
bootp3m1 
output_boot1 <- t(rbind(bootp3m1$t0, apply(bootp3m1$t, 2, function(x) sd(x))))  
stargazer(output_boot1, type = "html", title = "Tabla #", out = "bootp3m1.html")

#Guardar los betas
b1_1 <- bootp3m1$t0[1]
b2_1 <- bootp3m1$t0[2]
b3_1 <- bootp3m1$t0[3]
b4_1 <- bootp3m1$t0[4]
b5_1 <- bootp3m1$t0[5]
b6_1 <- bootp3m1$t0[6]

#Betas máximos
alpha <- 0.05

b1_1_max <- bootp3m1$t0[1]+qnorm(1-(alpha/2))*output_boot1[1,2]
b2_1_max <- bootp3m1$t0[2]+qnorm(1-(alpha/2))*output_boot1[2,2]
b3_1_max <- bootp3m1$t0[3]+qnorm(1-(alpha/2))*output_boot1[3,2]
b4_1_max <- bootp3m1$t0[4]+qnorm(1-(alpha/2))*output_boot1[4,2]
b5_1_max <- bootp3m1$t0[5]+qnorm(1-(alpha/2))*output_boot1[5,2]
b6_1_max <- bootp3m1$t0[6]+qnorm(1-(alpha/2))*output_boot1[6,2]

#Betas mínimos
b1_1_min <- bootp3m1$t0[1]-qnorm(1-(alpha/2))*output_boot1[1,2]
b2_1_min <- bootp3m1$t0[2]-qnorm(1-(alpha/2))*output_boot1[2,2]
b3_1_min <- bootp3m1$t0[3]-qnorm(1-(alpha/2))*output_boot1[3,2]
b4_1_min <- bootp3m1$t0[4]-qnorm(1-(alpha/2))*output_boot1[4,2]
b5_1_min <- bootp3m1$t0[5]-qnorm(1-(alpha/2))*output_boot1[5,2]
b6_1_min <- bootp3m1$t0[6]-qnorm(1-(alpha/2))*output_boot1[6,2]


IC1 <- data.frame()
#Mínimos
IC1[1,1] <- b1_1_min
IC1[2,1] <- b2_1_min
IC1[3,1] <- b3_1_min
IC1[4,1] <- b4_1_min
IC1[5,1] <- b5_1_min
IC1[6,1] <- b6_1_min
#Promedio
IC1[1,2] <- b1_1
IC1[2,2] <- b2_1
IC1[3,2] <- b3_1
IC1[4,2] <- b4_1
IC1[5,2] <- b5_1
IC1[6,2] <- b6_1
#Máximo
IC1[1,3] <- b1_1_max
IC1[2,3] <- b2_1_max
IC1[3,3] <- b3_1_max
IC1[4,3] <- b4_1_max
IC1[5,3] <- b5_1_max
IC1[6,3] <- b6_1_max

colnames(IC1) <- c("2.5%", "Promedio", "97.5%")
write_xlsx(IC1, "IC1.xlsx")

#Bootstrap para age_peak de hombres
eta.fn_h<-function(data,index){
  
  f<-lm(log_y ~ female + age + age2  + age_female + age2_female, data = df2_2, weights = fex_c/12, subset = index)
  coefs_11 <- f$coefficients
  b3<-coefs_11[3]
  b4<-coefs_11[4]
  age_peak_nolog<- -b3/(2*b4)
  return(age_peak_nolog)
  
}

# boot(data, eta_func, R=N)
booth <- boot(df2_2, eta.fn_h, R = 1000)
booth
output_booth <- t(rbind(booth$t0, apply(booth$t, 2, function(x) sd(x))))  
ap_min_h <- output_booth[1,1]- qnorm(1-(alpha/2))*output_booth[1,2]
ap_max_h <- output_booth[1,1]+ qnorm(1-(alpha/2))*output_booth[1,2]


#Bootstrap para age_peak de mujeres
eta.fn_m<-function(data,index){
  
  f<-lm(log_y ~ female + age + age2  + age_female + age2_female, data = df2_2, weights = fex_c/12, subset = index)
  coefs_11 <- f$coefficients
  b3<-coefs_11[3]
  b4<-coefs_11[4]
  b5<-coefs_11[5]
  b6<-coefs_11[6]
  age_peak_nolog<- -(b3+b5)/(2*(b4+b6))
  return(age_peak_nolog)
  
}

# boot(data, eta_func, R=N)
bootm <- boot(df2_2, eta.fn_m, R = 1000)
bootm
output_bootm <- t(rbind(bootm$t0, apply(bootm$t, 2, function(x) sd(x))))  
ap_min_m <- output_bootm[1,1]- qnorm(1-(alpha/2))*output_bootm[1,2]
ap_max_m <- output_bootm[1,1]+ qnorm(1-(alpha/2))*output_bootm[1,2]


#Gráficos
df2_2 <- df2_2%>%mutate(logy_min = b1_1_min + b2_1_min*female + b3_1_min*age+b4_1_min*age2+b5_1_min*age_female+b6_1_min*age2_female)
df2_2 <- df2_2%>%mutate(logy_avg = b1_1 + b2_1*female + b3_1*age+b4_1*age2+b5_1*age_female+b6_1*age2_female)
df2_2 <- df2_2%>%mutate(logy_max = b1_1_max + b2_1_max*female + b3_1_max*age+b4_1_max*age2+b5_1_max*age_female+b6_1_max*age2_female)

# gráfico solo con valores predichos 
ggplot(df2_2, aes(x = age, y = logy_avg, group=as.factor(female), color=as.factor(female))  ) +
  geom_point()+
  ggtitle("Ingreso laboral según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso laboral")+
  geom_vline(xintercept = output_bootm[1,1], linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_booth[1,1], linetype="dotted", color = "blue", size=1)+
  theme_classic()+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14), legend.text =  element_text(size = 18), legend.title  =  element_text(size = 20))


#Con los intervalos de los betas
ggplot(df2_2) +
  geom_point(aes(x = age, y = logy_avg, group=as.factor(female), color=as.factor(female)))+
  geom_line(aes(x = age, y = logy_min, group=as.factor(female), color=as.factor(female)))+
  geom_line(aes(x = age, y = logy_max, group=as.factor(female), color=as.factor(female)))+
  ggtitle("Ingreso laboral según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso laboral")+
  theme_classic()+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14), legend.text =  element_text(size = 18), legend.title  =  element_text(size = 20))


#Se van a construir los intervalos de confianza por medio de un bootstrap que calcule los valores predichos para cada sample
#Luego se obtiene la distribución de cada valor predicho en las distintas muestras y se calculan los intervalos de confianza

eta.fn_ic<-function(data,index){
  
  f<-lm(log_y ~ female + age + age2  + age_female + age2_female, data = df2_2, weights = fex_c/12, subset = index)
  pred <- f$fitted.values
  return(pred)
  
}

bootic <- boot(df2_2, eta.fn_ic, R = 1000)
bootic
output_bootic <- t(rbind(bootic$t0, apply(bootic$t, 2, function(x) sd(x))))  
output_bootic <- as.data.frame(output_bootic)
output_bootic <- output_bootic%>%mutate(max = V1 + qnorm(1-(alpha))*V2)
output_bootic <- output_bootic%>%mutate(min =  V1 - qnorm(1-(alpha))*V2)

output_bootic <- cbind(output_bootic, df2_2$female, df2_2$age)
colnames(output_bootic) <- c("y", "sd", "max", "min", "female", "age")

#Con esta gráfica queremos confirmar que el bootstrap esté retornando los mismos valores promedio que el gráfico original de predicciones
ggplot(output_bootic, aes(x = age, y = y, group=as.factor(female), color=as.factor(female))  ) +
  geom_point()+
  ggtitle("Ingreso laboral según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso laboral")+
  geom_vline(xintercept = output_bootm[1,1], linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_booth[1,1], linetype="dotted", color = "blue", size=1)+
  theme_classic()+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14), legend.text =  element_text(size = 18), legend.title  =  element_text(size = 20))

#Gráfico con intervalos de confianza para el salario a lo largo del trayecto de vida.
ggplot(output_bootic) +
  geom_point(aes(x = age, y = y, group=as.factor(female), color=as.factor(female)))+
  geom_smooth(aes(x = age, y = min, group=as.factor(female), color=as.factor(female)))+
  geom_smooth(aes(x = age, y = max, group=as.factor(female), color=as.factor(female)))+
  ggtitle("Ingreso laboral según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso laboral")+
  theme_classic()+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14), legend.text =  element_text(size = 18), legend.title  =  element_text(size = 20))


#Cambios
#hombres 23
b3_1+2*b4_1*23
#mujeres 23
(b3_1+b5_1)+2*(b4_1+b6_1)*23

#hombres 41
b3_1+2*b4_1*38
#mujeres 41
(b3_1+b5_1)+2*(b4_1+b6_1)*38


#Inicia el proceso con controles
#revisión de Na's en las variables escogidas
#Se crea una variable que toma valor de 1 si tiene una segunda activiad y 0 de lo contrario
df2_2 <- df2_2%>%mutate(seg_act = ifelse(is.na(p7050) == FALSE,1,0))
#Nivel educativo
is.na(df2_2$maxEducLevel)%>%table() #Tiene 1 na
#relación laboral
is.na(df2_2$relab)%>%table() #No tiene Na
#Oficio
is.na(df2_2$oficio)%>%table() #No tiene Na
#horas trabajadas
is.na(df2_2$hoursWorkUsual)%>%table() #No tiene Na
#formal
is.na(df2_2$formal)%>%table() #No tiene NA
#ingresos por arriendos o pensiones 
is.na(df2_2$p7495)%>%table() #No tiene NA
df2_2 <- df2_2%>%mutate(arr_pen = p7495 - 1)
df2_2$arr_pen
#Tamaño de la firma
is.na(df2_2$sizeFirm)%>%table() #No tiene NA

#Se elimina aquella observación con dato faltante para nivel educativo
df2_3 <- df2_2%>%subset(is.na(maxEducLevel)==FALSE)

#Se corre la regresión normalita (long regression)
p3m1c <- lm(data = df2_3, log_y~female+age+age2+age_female+age2_female + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) +factor(relab)+ hoursWorkUsual + sizeFirm, weights = fex_c/12)
summary(p3m1c)

#Se corre la misma regresión con el fe
incond <- lm(data = df2_3, log_y~female+age+age2+age_female+age2_female, weights = df2_3$fex_c/12)
summary(incond)


p3m2c <- felm(data = df2_3, log_y~female+age+age2+age_female+age2_female + hoursWorkUsual | factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = df2_3$fex_c/12)
summary(p3m2c)

p3m3c <- felm(data = df2_3, log_y~female+age+age2+age_female+age2_female | factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = df2_3$fex_c/12)
summary(p3m3c)

stargazer(incond, p3m2c, p3m3c, type = "html", title = "Regresión condicional", out = "p3c.html")


#Betas
#Diferencia porcentual de salario entre hombres y mujeres
#23 años
#Modelo 1
incond$coefficients[2]+ incond$coefficients[5]*23+incond$coefficients[6]*(23*23)
#Modelo2
p3m2c$coefficients[1]+ p3m2c$coefficients[4]*23+p3m2c$coefficients[5]*(23*23)
#Modelo3
p3m3c$coefficients[1]+ p3m3c$coefficients[4]*23+p3m3c$coefficients[5]*(23*23)


#39 años

incond$coefficients[2]+ incond$coefficients[5]*39+incond$coefficients[6]*(39*39)
#Modelo2
p3m2c$coefficients[1]+ p3m2c$coefficients[4]*39+p3m2c$coefficients[5]*(39*39)
#Modelo3
p3m3c$coefficients[1]+ p3m3c$coefficients[4]*39+p3m3c$coefficients[5]*(39*39)

#Aumentos marginales en la edad
#modelo1
incond$coefficients[3]+2*incond$coefficients[4]*23
incond$coefficients[3]+incond$coefficients[5]+2*(incond$coefficients[4]+incond$coefficients[6])*23

incond$coefficients[3]+2*incond$coefficients[4]*39
incond$coefficients[3]+incond$coefficients[5]+2*(incond$coefficients[4]+incond$coefficients[6])*39

#modelo2
p3m2c$coefficients[2]+2*p3m2c$coefficients[3]*23
p3m2c$coefficients[2]+p3m2c$coefficients[4]+2*(p3m2c$coefficients[3]+p3m2c$coefficients[5])*23


p3m2c$coefficients[2]+2*p3m2c$coefficients[3]*39
p3m2c$coefficients[2]+p3m2c$coefficients[4]+2*(p3m2c$coefficients[3]+p3m2c$coefficients[5])*39



#modelo3
p3m3c$coefficients[2]+2*p3m3c$coefficients[3]*23
p3m3c$coefficients[2]+p3m3c$coefficients[4]+2*(p3m3c$coefficients[3]+p3m3c$coefficients[5])*23


p3m3c$coefficients[2]+2*p3m3c$coefficients[3]*39
p3m3c$coefficients[2]+p3m3c$coefficients[4]+2*(p3m3c$coefficients[3]+p3m3c$coefficients[5])*39



#Se calcula para los 3 modelos el intervalo de confianza de agepeak
#Modelo 2

#Bootstrap para age_peak de hombres
eta.fn_h2<-function(data,index){
  
  f<-felm(data = df2_3, log_y~female+age+age2+age_female+age2_female + hoursWorkUsual | factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = df2_3$fex_c/12, subset = index)
  coefs_11 <- f$coefficients
  b3<-coefs_11[2]
  b4<-coefs_11[3]
  age_peak_nolog<- -b3/(2*b4)
  return(age_peak_nolog)
  
}

# boot(data, eta_func, R=N)
booth2 <- boot(df2_3, eta.fn_h2, R = 1000)
booth2
output_booth2 <- t(rbind(booth2$t0, apply(booth2$t, 2, function(x) sd(x))))  
ap_min_h2 <- output_booth2[1,1]- qnorm(1-(alpha/2))*output_booth2[1,2]
ap_max_h2 <- output_booth2[1,1]+ qnorm(1-(alpha/2))*output_booth2[1,2]


#Bootstrap para age_peak de mujeres
eta.fn_m2<-function(data,index){
  
  f<-felm(data = df2_3, log_y~female+age+age2+age_female+age2_female + hoursWorkUsual | factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = df2_3$fex_c/12, subset = index)
  coefs_11 <- f$coefficients
  b3<-coefs_11[2]
  b4<-coefs_11[3]
  b5<-coefs_11[4]
  b6<-coefs_11[5]
  age_peak_nolog<- -(b3+b5)/(2*(b4+b6))
  return(age_peak_nolog)
  
}

# boot(data, eta_func, R=N)
bootm2 <- boot(df2_3, eta.fn_m2, R = 1000)
bootm2
output_bootm2 <- t(rbind(bootm2$t0, apply(bootm2$t, 2, function(x) sd(x))))  
ap_min_m2 <- output_bootm2[1,1]- qnorm(1-(alpha/2))*output_bootm2[1,2]
ap_max_m2 <- output_bootm2[1,1]+ qnorm(1-(alpha/2))*output_bootm2[1,2]


#Modelo 3

#Bootstrap para age_peak de hombres
eta.fn_h3<-function(data,index){
  
  f<-felm(data = df2_3, log_y~female+age+age2+age_female+age2_female | factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = df2_3$fex_c/12, subset = index)
  coefs_11 <- f$coefficients
  b3<-coefs_11[2]
  b4<-coefs_11[3]
  age_peak_nolog<- -b3/(2*b4)
  return(age_peak_nolog)
  
}

# boot(data, eta_func, R=N)
booth3 <- boot(df2_3, eta.fn_h3, R = 1000)
booth3
output_booth3 <- t(rbind(booth3$t0, apply(booth3$t, 2, function(x) sd(x))))  
ap_min_h3 <- output_booth3[1,1]- qnorm(1-(alpha/2))*output_booth3[1,2]
ap_max_h3 <- output_booth3[1,1]+ qnorm(1-(alpha/2))*output_booth3[1,2]


#Bootstrap para age_peak de mujeres
eta.fn_m3<-function(data,index){
  
  f<-felm(data = df2_3, log_y~female+age+age2+age_female+age2_female | factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = df2_3$fex_c/12, subset = index)
  coefs_11 <- f$coefficients
  b3<-coefs_11[2]
  b4<-coefs_11[3]
  b5<-coefs_11[4]
  b6<-coefs_11[5]
  age_peak_nolog<- -(b3+b5)/(2*(b4+b6))
  return(age_peak_nolog)
  
}

# boot(data, eta_func, R=N)
bootm3 <- boot(df2_3, eta.fn_m3, R = 1000)
bootm3
output_bootm3 <- t(rbind(bootm3$t0, apply(bootm3$t, 2, function(x) sd(x))))  
ap_min_m3 <- output_bootm3[1,1]- qnorm(1-(alpha/2))*output_bootm3[1,2]
ap_max_m3 <- output_bootm3[1,1]+ qnorm(1-(alpha/2))*output_bootm3[1,2]


#Predicciones modelo 2 y 3

#Modelo 2
df2_3$modelo2 <- predict(lm(data = df2_3, log_y~female+age+age2+age_female+age2_female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = df2_3$fex_c/12), newdata = df2_3)
#Modelo 3
df2_3$modelo3 <- predict(lm(data = df2_3, log_y~female+age+age2+age_female+age2_female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = df2_3$fex_c/12), newdata = df2_3)


#Gráfico modelo2
ggplot(df2_3, aes(x = age, y = modelo2, group=as.factor(female), color=as.factor(female))  ) +
  geom_point()+
  ggtitle("Ingreso laboral según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso laboral")+
  geom_vline(xintercept = output_bootm2[1,1], linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_booth2[1,1], linetype="dotted", color = "blue", size=1)+
  theme_classic()+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14), legend.text =  element_text(size = 18), legend.title  =  element_text(size = 20))

#Gráfico modelo3
ggplot(df2_3, aes(x = age, y = modelo3, group=as.factor(female), color=as.factor(female))  ) +
  geom_point()+
  ggtitle("Ingreso laboral según la edad, por sexo")+
  xlab("Edad")+
  labs(colour = "Sexo")+
  ylab("Logaritmo del ingreso laboral")+
  geom_vline(xintercept = output_bootm3[1,1], linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = output_booth3[1,1], linetype="dotted", color = "blue", size=1)+
  theme_classic()+
  scale_colour_manual(values=c('0'='blue', '1'='red'), label= c('0'='Hombre', '1'='Mujer') )+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.title.x = element_text(hjust = 0.5, size = 16), axis.title.y = element_text(hjust = 0.5, size = 16), axis.text = element_text(size = 14), legend.text =  element_text(size = 18), legend.title  =  element_text(size = 20))



#Ignacio dijo que se debía calcular de forma manual el FWL

#Para el modelo que tiene todos los controles
#Modelo 2

#FWL con felm()
p3m2c <- felm(data = df2_3, log_y~female+age+age2+age_female+age2_female + hoursWorkUsual | factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = df2_3$fex_c/12)
summary(p3m2c)

#Regresión lineal larga
p3m2clm <- lm(data = df2_3, log_y~female+age+age2+age_female+age2_female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = df2_3$fex_c/12)
summary(p3m2clm)

# fwl manual para female
df2_3 <- df2_3%>%mutate(res_y_f = lm(data = df2_3, log_y~ age+age2+age_female+age2_female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = fex_c/12)$residuals,
                        res_x_f = lm(data = df2_3, female ~ age+age2+age_female+age2_female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = fex_c/12)$residuals)

modfemale <- lm(res_y_f~res_x_f-1,df2_3, weights = fex_c/12)
summary(modfemale)

# fwl manual para age_female
df2_3 <- df2_3%>%mutate(res_y_af = lm(data = df2_3, log_y~ age+age2+female+age2_female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = fex_c/12)$residuals,
                        res_x_af = lm(data = df2_3, age_female ~ age+age2+female+age2_female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = fex_c/12)$residuals)

modfemaleage <- lm(res_y_af~res_x_af-1,df2_3, weights = fex_c/12)
summary(modfemaleage)

# fwl manual para age2_female
df2_3 <- df2_3%>%mutate(res_y_a2f = lm(data = df2_3, log_y~ age+age2+age_female+female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = fex_c/12)$residuals,
                        res_x_a2f = lm(data = df2_3, age2_female ~ age+age2+age_female+female + hoursWorkUsual + factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = fex_c/12)$residuals)

modfemaleage2 <- lm(res_y_a2f~res_x_a2f-1,df2_3, weights = fex_c/12)
summary(modfemaleage2)


#Modelo 3
#fwl
p3m3c <- felm(data = df2_3, log_y~female+age+age2+age_female+age2_female | factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = df2_3$fex_c/12)
summary(p3m3c)
#Long regression
p3m3clm <- felm(data = df2_3, log_y~female+age+age2+age_female+age2_female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = df2_3$fex_c/12)
summary(p3m3clm)


# fwl manual para female
df2_3 <- df2_3%>%mutate(res_y_f3 = lm(data = df2_3, log_y~ age+age2+age_female+age2_female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = fex_c/12)$residuals,
                        res_x_f3 = lm(data = df2_3, female ~ age+age2+age_female+age2_female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = fex_c/12)$residuals)

modfemale3 <- lm(res_y_f3~res_x_f3-1,df2_3, weights = fex_c/12)
summary(modfemale3)

# fwl manual para age_female
df2_3 <- df2_3%>%mutate(res_y_af3 = lm(data = df2_3, log_y~ age+age2+female+age2_female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = fex_c/12)$residuals,
                        res_x_af3 = lm(data = df2_3, age_female ~ age+age2+female+age2_female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = fex_c/12)$residuals)

modfemaleage3 <- lm(res_y_af3~res_x_af3-1,df2_3, weights = fex_c/12)
summary(modfemaleage3)

# fwl manual para age2_female
df2_3 <- df2_3%>%mutate(res_y_a2f3 = lm(data = df2_3, log_y~ age+age2+age_female+female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = fex_c/12)$residuals,
                        res_x_a2f3 = lm(data = df2_3, age2_female ~ age+age2+age_female+female + factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = fex_c/12)$residuals)

modfemaleage23 <- lm(res_y_a2f3~res_x_a2f3-1,df2_3, weights = fex_c/12)
summary(modfemaleage23)



#Por último, se debe hacer un bootstrap para contar con los errores robustos

#Modelo 2

eta.fn_p3m2<-function(data,index){
  coef(felm(data = df2_3, log_y~female+age+age2+age_female+age2_female + hoursWorkUsual | factor(sizeFirm) + factor(seg_act) + factor(maxEducLevel) + factor(oficio) + factor(formal) + factor(relab), weights = df2_3$fex_c/12, subset = index))
}

# boot(data, eta_func, R=N)
bootp3m2 <- boot(df2_3, eta.fn_p3m2, R = 1000)
bootp3m2 
output_bootm2 <- t(rbind(bootp3m2$t0, apply(bootp3m2$t, 2, function(x) sd(x))))  
stargazer(output_bootm2, type = "html", title = "Tabla #", out = "bootp3m2fw.html")


#Se calcula female de forma manual
eta.fn_manual<-function(data,index){
  coef(lm(res_y_f~res_x_f-1,df2_3, weights = fex_c/12, subset = index))
}

# boot(data, eta_func, R=N)
bootpmanual <- boot(df2_3, eta.fn_manual, R = 1000)
bootpmanual

#Modelo3
eta.fn_p3m3<-function(data,index){
  coef(felm(data = df2_3, log_y~female+age+age2+age_female+age2_female | factor(seg_act) + factor(maxEducLevel) + factor(formal), weights = df2_3$fex_c/12, subset = index))
}


# boot(data, eta_func, R=N)
bootp3m3 <- boot(df2_3, eta.fn_p3m3, R = 1000)
bootp3m3 
output_bootm3 <- t(rbind(bootp3m2$t0, apply(bootp3m2$t, 2, function(x) sd(x))))  
stargazer(output_bootm2, type = "html", title = "Tabla #", out = "bootp3m2fw.html")


eta.fn_manual3<-function(data,index){
  coef(lm(res_y_f3~res_x_f3-1,df2_3, weights = fex_c/12, subset = index))
}

# boot(data, eta_func, R=N)
bootpmanual3 <- boot(df2_3, eta.fn_manual3, R = 1000)
bootpmanual3



####Pruebas adicionales
plot(df2$y_total_m, df2$ingtot)
is.na(df2$y_total_m)%>%table()
is.na(df2$y_ingLab_m)%>%table()
is.na(df2$ingtot)%>%table()


plot(df2_3$sizeFirm, df2_3$y_total_m)


df2_3%>%group_by(female)%>%summarise(mean = mean(hoursWorkUsual))
df2_3%>%count(sizeFirm, wt = fex_c/12)
df2%>%count(sizeFirm, wt = fex_c/12)


df2_3%>%group_by(female)%>%count(formal, wt = fex_c/12)%>%mutate(perc = n/sum(n))


eta.fn_icp2<-function(data,index){
  
  f<-lm(log_y ~ age + age2, data = df2_2, weights = fex_c/12, subset = index)
  pred <- f$fitted.values
  return(pred)
  
}

booticp2 <- boot(df2_2, eta.fn_icp2, R = 1000)
booticp2

