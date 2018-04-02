#Multiple Regression
#Data: https://archive.ics.uci.edu/ml/datasets/automobile
#This data set consists of three types of entities:
#(a) the specification of an auto in terms of various characteristics, 
#(b) its assigned insurance risk rating, 
#(c) its normalized losses in use as compared to other cars. 

#Regresion linear, regresion local, dependencia entre variables y diseño de modelos para predicciones. 


#Prediccion de los precios con base en las especificaciones del automovil. 
rm(list=ls()) 

#install.packages("Hmisc",dependencies = TRUE)
#install.packages("psych",dependencies = TRUE)
#install.packages("car",dependencies = TRUE)
#install.packages("ggplot2",dependencies = TRUE)
#install.packages("rgl",dependencies = TRUE)

library(Hmisc)
library(psych)
library(car)
library(ggplot2)
library(rgl)
library(gridExtra)
library(scales)

#Definir el directorio de trabajo
setwd("...")
auto<-read.csv(file="automobile_data.csv",header=TRUE,na.strings = "?")
head(auto)
View(auto)
summary(auto)

summary(auto$price)
summary(auto$normalized.losses)
summary(auto$symboling)
summary(auto$num.of.doors)
summary(auto$horsepower)
summary(auto$peak.rpm)
summary(auto$bore)
summary(auto$stroke)

##Limpieza de las variables, NA.

#Variable a predecir (PRECIO)
auto$price<-as.numeric(impute(auto$price,mean))
auto$normalized.losses<-as.numeric(impute(auto$normalized.losses,mean))
auto$symboling<-as.numeric(impute(auto$symboling,median))

#Variables con valores faltantes
#Cuatro puertas ==> 1, Dos puertas ==>2
auto$num.of.doors<-as.numeric(impute(auto$num.of.doors,median))
auto$horsepower<-as.numeric(impute(auto$horsepower,mean))
auto$peak.rpm<-as.numeric(impute(auto$peak.rpm,mean))
auto$bore<-as.numeric(impute(auto$bore,mean))
auto$stroke<-as.numeric(impute(auto$stroke,mean))

summary(auto)
summary(auto$price)
summary(auto$normalized.losses)
summary(auto$symboling)
summary(auto$num.of.doors)
summary(auto$horsepower)
summary(auto$peak.rpm)
summary(auto$bore)
summary(auto$stroke)

##Examinar la relacion entre dos variables de importancia, la potencia y el precio del auto. 
#Precio=Intercept+horsepower.coeff+Horsepower
ggplot(auto,aes(x=horsepower,y=price))+
  ggtitle("Caballos de fuerza vs Precio")+
  geom_point(color="lightslateblue",size=1)+
  stat_smooth(method = "lm",color="red",se=FALSE,fullrange = TRUE,size=0.5)
#El modelo lineal se obtiene: precio=m*horsepower+b. y=mx+b.


#Generacion de muestras para llevar a cabo el training. 
set.seed(5)
sample.size<-0.7

#Generacion de muestras aleatorias y permutaciones.
#Multiplica el tamaño de la poblacion por el tamaño de la muestra y lo redondea.
#round(length(auto$price)*sample.size) 
#Selecciona el 70% de la poblacion de auto para muestras.
s1<-auto[sample.int(length(auto$price),round(length(auto$price)*sample.size)),]
s2<-auto[sample.int(length(auto$price),round(length(auto$price)*sample.size)),]
s3<-auto[sample.int(length(auto$price),round(length(auto$price)*sample.size)),]
s4<-auto[sample.int(length(auto$price),round(length(auto$price)*sample.size)),]
s5<-auto[sample.int(length(auto$price),round(length(auto$price)*sample.size)),]
s6<-auto[sample.int(length(auto$price),round(length(auto$price)*sample.size)),]

##Generacion de 6 muestras diferentes para comparar como el modelo se modifica de acuerdo a las
#muestras obtenidas aleatoriamente.
p1 <- ggplot(auto, aes(x=horsepower, y=price)) + ggtitle("(P1) HP vs. Precio") + 
      geom_point(color="gold", size=2) + geom_point(data=s1, color="black", size=1.5) +
      stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=1, data=s1)

p2 <- ggplot(auto, aes(x=horsepower, y=price)) + ggtitle("(P2) HP vs. Precio") + 
      geom_point(color="green", size=2) + geom_point(data=s2, color="black", size=1.5) +
      stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=1, data=s2)

p3 <- ggplot(auto, aes(x=horsepower, y=price)) + ggtitle("(P3) HP vs. Precio") + 
      geom_point(color="red", size=2) + geom_point(data=s3, color="black", size=1.5) +
      stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=1, data=s3)

p4 <- ggplot(auto, aes(x=horsepower, y=price)) + ggtitle("(P4) HP vs. Precio") + 
      geom_point(color="deepskyblue", size=2) + geom_point(data=s4, color="black", size=1.5) +
      stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=1, data=s4)

p5 <- ggplot(auto, aes(x=horsepower, y=price)) + ggtitle("(P5) HP vs. Precio") + 
      geom_point(color="magenta", size=2) + geom_point(data=s5, color="black", size=1.5) +
      stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=1, data=s5)

p6 <- ggplot(auto, aes(x=horsepower, y=price)) + ggtitle("(P6) HP vs. Precio") + 
      geom_point(color="brown", size=2) + geom_point(data=s6, color="black", size=1.5) +
      stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=1, data=s6)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2)

  # Graficar los seis modelos generados en una misma grafica para su mejor comparacion. 
  ggplot(auto, aes(x=horsepower, y=price)) + 
  ggtitle("HP vs. Precio") +geom_hline(yintercept=mean(auto$price), color="black") +
  geom_point(color="gray60", size=1) +
  stat_smooth(method="lm", aes(colour = 'S1'),  se=FALSE, fullrange=TRUE, size=1, data=s1) +
  stat_smooth(method="lm", aes(colour = 'S2'), se=FALSE, fullrange=TRUE, size=1, data=s2) +
  stat_smooth(method="lm", aes(colour = 'S3'), se=FALSE, fullrange=TRUE, size=1, data=s3) +
  stat_smooth(method="lm", aes(colour = 'S4'), se=FALSE, fullrange=TRUE, size=1, data=s4) +
  stat_smooth(method="lm", aes(colour = 'S5'), se=FALSE, fullrange=TRUE, size=1, data=s5) +
  stat_smooth(method="lm", aes(colour = 'S6'), se=FALSE, fullrange=TRUE, size=1, data=s6) +
  stat_smooth(method="lm", aes(colour = 'S0') , se=FALSE, fullrange=TRUE, size=1) +
  scale_colour_manual(values=c("S1"="gold", "S2"="red","S3"="khaki","S4"="blue",
                                 "S5"="magenta","S6"="brown","S0"="green"))
#Todas las lineas de los modelos son cercanas pero no las mismas que la linea S0 
  #realizada con todas las muestras.

  # Regresion lineal vs LOWESS (locally weighted scatterplot smoothing) regression.
  # Agregar el intervalo de confidencia alredor de cada modelo. 
  # Plot all data points and a regression line 
  ggplot(auto, aes(x=horsepower, y=price)) + 
    ggtitle("HP vs Precio") +
    geom_point(color="gray", size=1) +
    stat_smooth(method="lm", size=0.5, color="blue")
  
  # Analizar la relacion de la variable de perdidas con respecto al precio, 
  # usando un modelo polinomial.
  ggplot(auto, aes(x=normalized.losses, y=price)) + 
    ggtitle("Perdidas normalizadas vs Precio") +
    geom_point(color="gray", size=1) + 
    stat_smooth(method="loess", size=0.5, color="red")
  
  # Comparacion directa entre el modelo lineal y el modelo local. 
  ggplot(auto, aes(x=horsepower, y=price)) + 
    ggtitle("HP vs Precio") +
    geom_point(color="gray", size=1) + 
    stat_smooth(method="lm", size=0.5, color="blue") +
    stat_smooth(method="loess", size=0.5, color="red")
  
  # Llevando a cabo una inspeccion visual, el modelo lineal es aceptable en su comportamiento.
  
  ##### Or we can create the linear model and its plot it
  # We will create a training and validation samples
  set.seed(5)
  train.size <- 0.7
  train.index <- sample.int(length(auto$price), round(length(auto$price) * train.size))
  train.sample <- auto[train.index,]
  valid.sample <- auto[-train.index,] #Subconjunto para validacion
  
  ### Creacion del modelo lineal, empleando el subconjunto de entrenamiento.
  fit <- lm(price ~ horsepower, data=train.sample)
  fit
  
  # Hayar todas las predicciones para los valores de los conjuntos de validacion y entrenamiento. 
  train.sample$Pred.price <- predict(fit)
  valid.sample$Pred.price <- predict(fit,newdata = subset(valid.sample, select=c(price, horsepower)))

  
  # Detalles sobre la confiabilidad del modelo aplicado al subconjunto de entrenamiento. 
  summary(fit)
  
  # Check how good is the model on the training set - correlation^2, RME and MAE
  #Verificacion de las predicciones del modelo usando la correlacion, RME (Error cuadrático medio).
  #MAE, error absoluto medio.
  #https://medium.com/human-in-a-machine-world/mae-and-rmse-which-metric-is-better-e60ac3bde13d
  
  train.corr <- round(cor(train.sample$Pred.price, train.sample$price), 2)
  train.RMSE <- round(sqrt(mean((train.sample$Pred.price - train.sample$price)^2)))
  train.MAE <- round(mean(abs(train.sample$Pred.price - train.sample$price)))
  
  c(train.corr^2, train.RMSE, train.MAE)
  
  # Validacion del modelo pero ahora con el subconjunto de validacion. 
  valid.corr <- round(cor(valid.sample$Pred.price, valid.sample$price), 2)
  valid.RMSE <- round(sqrt(mean((valid.sample$Pred.price - valid.sample$price)^2)))
  valid.MAE <- round(mean(abs(valid.sample$Pred.price - valid.sample$price)))
  c(valid.corr^2, valid.RMSE, valid.MAE)
  
  # Grafica final con toda la informacion
  ggplot(auto, aes(x=horsepower, y=price)) + 
    ggtitle("Potencia Motor vs precio") +
    theme(legend.position='none') +
    labs(x="Potencia (HP)", y="Precio ($)") +
    geom_point(color="gray", size=1) +
    stat_smooth(method="lm", color="darkgray", size=0.5) + # se=FALSE is no band is requied
    xlim(min(train.sample$horsepower), 
         max(train.sample$horsepower)) +
    geom_segment(data=data.frame(horsepower=valid.sample$horsepower, 
                                 price=valid.sample$Pred.price), 
                 aes(x=valid.sample$horsepower, xend=valid.sample$horsepower, 
                     y=valid.sample$price, yend=valid.sample$Pred.price, 
                     color="orange"),
                 size=0.5) +
    geom_point(data=data.frame(horsepower=valid.sample$horsepower, 
                               price=valid.sample$price),
               color="orange", size=1.5) +
    geom_line(data=data.frame(horsepower=train.sample$horsepower, 
                              price=train.sample$Pred.price), 
              color="blue", size=0.5)
  
  # Final del programa
  
  
  
  
  
  




