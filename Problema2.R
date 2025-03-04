#Codigo para problema 2

mis_dades <- iris; mis_dades
x <- mis_dades$Petal.Length; x
mean(x)  #Media
sd(x)  #Desviación estándar
hist(x)   #Histograma
y <- mis_dades$Sepal.Length
mean(y)
plot(x,y)  #Gráfico puntos x,y

#Formula Pendiente m
m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2); m

#Formula b (Termino independiente)
b <- mean(y)- m*mean(x); b

#Prediccion en 1.5
w <- m*1.5+b; w


#####

mod <- lm(y~x); mod
summary(mod)
data.frame(x=x)
ypred <- predict(mod,data.frame(x=x)) #Todos los valores de y
plot(x,y, col='red',pch=16)
lines(x,ypred) #Recta lineal

#Formula Multiple R^2
Rsq <- sum((ypred-mean(y))^2)/sum((y-mean(y))^2); Rsq


#Codigo para problema 2
