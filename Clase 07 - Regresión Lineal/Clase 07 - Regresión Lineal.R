##########################################################
#                   ESTADÍSTICA                          #
#                Profs. DEL ROSSO - NUSKE                #
#	            MAESTRÍA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERÍA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTADÍSTICA
## Docentes: Rodrigo Del Rosso - Gustavo Levinis

rm(list = ls())

#####################
## RUTA DE TRABAJO ##
#####################

path = "..."
setwd(path)

##############
## PAQUETES ##
##############

suppressPackageStartupMessages({
  library(PerformanceAnalytics)
  library(corrplot)
  library(Hmisc)
  library(lmtest)
})

#############################
## REGRESIÓN LINEAL SIMPLE ##
#############################

#########################
####### EJEMPLO 2 #######
#########################

y <-c(1,2,3,-1,0,-1,2,1,2)
x <-c(0,1,2,-2,1,-2,0,-1,1)

lm(y ~ x) 

summary(lm(y ~ x)) 

#########################
####### EJEMPLO 2 #######
#########################

datos <- read.csv("Advertising.csv",header = T)
attach(datos)

par(mfrow = c(3,1))
plot(TV,sales, col = "red")
plot(radio,sales, col = "red")
plot(newspaper,sales, col = "red")

cor.test(TV,sales)
cor.test(radio,sales)
cor.test(newspaper,sales)

res <- cor(datos[,-1])
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

chart.Correlation(datos[,-1],method="pearson",histogram=TRUE,pch=16)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

## ESTIMACIÓN DE DISTINTAS ESPECIFICACIONES ##
modelo1 = lm(sales ~ TV,data = datos)
summary(modelo1)

modelo2 = lm(sales ~ TV + radio,data = datos)
summary(modelo2)

modelo3 = lm(sales ~ TV + radio + newspaper,data = datos)
summary(modelo3)

## INTERVALO DE CONFIANZA ##
coef(modelo1)

confint(modelo1)
confint(modelo1,level = 0.99)
confint(modelo1,parm = "TV",level = 0.975)

## GRÁFICO CONJUNTO CON LA NUBE DE PUNTOS ##
plot(TV,sales, col = "red")
abline(lm(sales ~ TV),col = "blue")

## ANÁLISIS DE RESIDUOS ##
residuos1 = resid(modelo1)
residuos2 = resid(modelo2)
residuos3 = resid(modelo3)

summary(residuos1)
summary(residuos2)
summary(residuos3)

histogram(residuos1)
histogram(residuos2)
histogram(residuos3)

## TEST DE NORMALIDAD ##
tseries::jarque.bera.test(residuos1)
tseries::jarque.bera.test(residuos2)
tseries::jarque.bera.test(residuos3)

## ANÁLISIS VISUAL RESPECTO A LA MEDIA ##
plot(residuos1, type = "l")
abline(h = 0, col = "red")

plot(residuos2, type = "l")
abline(h = 0, col = "red")

plot(residuos3, type = "l")
abline(h = 0, col = "red")

## QQ-PLOT ##

qqnorm(residuos1)
qqline(residuos1)

qqnorm(residuos2)
qqline(residuos2)

qqnorm(residuos3)
qqline(residuos3)

## TEST DE AUTOCORRELACIÓN DE ORDEN 1 - DURBIN WATSON ##
lmtest::dwtest(modelo1)
lmtest::dwtest(modelo2)
lmtest::dwtest(modelo3)

## TEST DE HOMOCEDASTICIDAD - WHITE ##
lmtest::bptest(modelo1, varformula = ~ I(TV^2), data = datos)
lmtest::bptest(modelo2, varformula = ~ I(TV^2) + I(radio^2) + radio*TV, data = datos)
lmtest::bptest(modelo3, varformula = ~ I(TV^2) + I(radio^2) + I(newspaper^2) + radio*TV + radio*newspaper + TV*newspaper, data = datos)

## PREDICCIONES ##
predict(modelo1)  ## valores fitteados
fitted(modelo1)   ## da lo mismo

summary(TV)
TV_nuevos = c(150,200,102,231)
datos_nuevos = data.frame(TV = TV_nuevos)
predict(modelo1, newdata = datos_nuevos)

## DIAGNÓSTICO GRÁFICO ##
x11()
layout(matrix(1:4,2,2))    
plot(modelo1) 

#########################
####### EJEMPLO 3 #######
#########################

datos <- read.table("PWT_2000.txt",header=TRUE,sep="") 
attach(datos)

head(datos)

plot(K,PIBPCL,col="red")

cor(K,PIBPCL)
cor.test(K,PIBPCL)                     ## significatividad
cor.test(K,PIBPCL,alternative="less")  ## contraste unilateral contra menor

PWT <- lm(PIBPCL ~ K) 

coef(PWT)

summary(PWT)

abline(PWT,col = "blue")

hist(PIBPCL,main = "Histograma de PIBPCL", freq=FALSE) 
lines(density(PIBPCL), col = "red")

boxplot(PIBPCL,col="yellow")
title("Boxplot de PIBPCL")

layout(matrix(1:4,2,2))   ## diagnóstico gráfico
plot(PWT) 

#########################
####### EJEMPLO 4 #######
#########################

rm(list = ls())

deuda<-read.csv("deuda_mex.csv",header =T)

attach(deuda)

head(deuda)

summary(y)
summary(x2,x3)
summary(deuda)

cor(deuda) 

pairs(deuda)

modelo <- lm(y ~ x2 + x3) 

summary(modelo) # Analizamos la significación de la regresión

anova(modelo)   # otra forma

vcov(modelo)    # Matriz de Varianzas y Covarianzas de los Estimadores #

# Predicción

range(x2)
range(x3)
range(y)

modelo

1.381549 + 83 * 0.022279 + 22 *(-0.003898)

1.381549 + 85 * 0.022279 + 24 *(-0.003898)

predict(modelo)  ## predicción para cada uno de los valores observados de "y"

modelo$fitted.values  ## otra forma

# Chequeamos

round(predict(modelo)- modelo$fitted.values,0)

# Intervalo de predicción

x2<-c(83,95,125)
x3<-c(23,25,42)
new <- data.frame(x2,x3)
predict(modelo,new,interval = "prediction")

predict(modelo,new,interval = "prediction",level = 0.99)
predict(modelo,interval = "prediction")

# Intervalo de confianza
predict(modelo,new,interval = "confidence")         ## más preciso estimar una respuesta media que una respuesta global
predict(modelo,new,interval = "confidence",level=0.99)

# Ajustamos la recta de cuadrados mínimos

lm(y ~ x)

lm(y ~ x1 + x2 + x3)

lm(y ~ x1 + x2 + x1 * x2)

## x1 cuantitativa
## x2 factor

######################################
####### DIAGNÓSTICO DEL MODELO #######
######################################

residuos<-modelo$residuals

resid(modelo)

summary(residuos)

## NORMALIDAD ##

qqnorm(residuos,col="red")
qqline(residuos,col="blue")

shapiro.test(residuos)

library(tseries)
jarque.bera.test(residuos)

library(nortest)
ad.test(residuos)

## INCORRELACIÓN ##

library(lmtest)
dwtest(modelo)

## HOMOCEDASTICIDAD ##

bptest(modelo)

## ERROR DE ESPECIFICACIÓN ##

m<-2

resettest(modelo,power=2:m)

resettest(PWT)