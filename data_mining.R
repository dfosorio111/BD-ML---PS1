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
# browseURL("https://lectures-r.gitlab.io/big-data-202202/week-01/")

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


# 5. Modelos de Regresion Lineal

#relación ingreso y edad
# mutate(new_col=f(var)): crea nueva columna a partir de existentes
# mutate(x=var):permite crear nuevas variables a partir de otras variables para construir funciones f(y), f(x)
df2 <- df2%>%mutate(age2=age^2)
df2 <- df2%>%mutate(inglabo=impa+isa)


### Bootstraping ###
# boot(database, eta_fun, R=N):  permite obtener los estimadores t(beta) del modelo

eta.fn_1<-function(data,index){
  coef(lm(impa~age+age2, data = df2, weights = fex_c, subset = index))
}

# boot(data, eta_func, R=N)
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

# mutate(x=var):permite crear nuevas variables a partir de otras variables para construir funciones f(y), f(x)
# inglabo_hat(fun_predic), inglabo_hat_nolog(fun_predic)
df2 <- df2%>%mutate(inglabo_hat = exp(boot4$t0[1]+boot4$t0[2]*age+boot4$t0[3]*age2)-1)
df2 <- df2%>%mutate(inglabo_hat_nolog = boot3$t0[1]+boot3$t0[2]*age+boot3$t0[3]*age2)

# gráfico solo con valores predichos g5
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



#Age peak: igualar la función derivada de y = 0  
age_peak <- -(boot4$t0[2]/(2*boot4$t0[3]))
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
  coef(lm(log(inglabo) ~ female*age+female*age2, data = df2%>%subset(inglabo>0), weights = fex_c, subset = index))
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
  ggtitle("Ingreso total según la edad, por sexo")+
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



# modelo lineal - variables de control: maxEducLevel

modelo_gap1 <- lm(data = dfgap, y_total_m ~ age*female+age2*female+factor(maxEducLevel) , weights = fex_c)
summary(modelo_gap1)

# modelo lineal - variables de control: maxEducLevel, factor(estrato1)
modelo_gap2 <- lm(data = dfgap, y_total_m ~ age*female+age2*female+factor(maxEducLevel)+ factor(estrato1) , weights = fex_c)
summary(modelo_gap1)

# modelo lineal - variables de control: maxEducLevel, factor(estrato1)
modelo_gap3 <- lm(data = dfgap, y_total_m ~ age*female+age2*female+factor(maxEducLevel)+ factor(estrato1) , weights = fex_c)
summary(modelo_gap1)



# modelo lineal - variables de control: nivel educativo, estrato1, relab, oficio1
# basado en ncbi:literatura

modelo_gap4 <- lm(data = dfgap, y_total_m ~ age*female+age2*female+factor(maxEducLevel)+ factor(estrato1) + factor(relab) + factor(oficio), weights = fex_c)
summary(modelo_gap1)








# modelo fwl (matriz de proyección y annihilation)
modelo_gap1_fwl <- felm(y_total_m ~ age*female+age2*female| factor(maxEducLevel)+ factor(estrato1), data =dfgap)
summary(modelo_gap1_fwl)



# bootstping para modelo con fwl (matriz de proyección y annihilation)

eta.fn_gap1<-function(data,index){
  coef(lm(ingtot ~ female*age+female*age2, data = df2, weights = fex_c, subset = index))
}



# boot(data, eta_func, R=N)
bootgap1 <- boot(df2, eta.fngap1, R = 1000)






## Punto 4: Prediction and Performance Evaluation
# prediction, overfitting and cross-val

# split database into test-set and test-set

p_load(tidyverse, fabricatr, stargazer)

# set/crear seed para reproducibilidad
set.seed(101010)

# crear dataframe para fit/train el modelo

df_ml <- df2%>%mutate(holdout= as.logical(1:nrow(df2)%in%sample(nrow(df2), nrow(df2)*.3)) )
test_set <- df_ml[df_ml$holdout==T,]
train_set <- df_ml[df_ml$holdout==F,]




# contruir el modelo dummy/naive especificacion

spec1 <- lm()







