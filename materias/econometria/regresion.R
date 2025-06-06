#%%%%%% EJEMPLOS MODELO DE REGRESION LINEAL %%%%%%%%%%%%%%%%%
rm(list=ls())

#Paquetes a utilizar
library(dplyr)
library(wooldridge)
library(stargazer)

#####
# Respuestas Ejemplo 1
datos <- wooldridge::mroz

#a) Estimando los modelos


mod <- lm(hours ~ wage+educ+kidslt6+kidsge6, data= datos)


#Reportando los resultados
stargazer(mod, type = "text",
          colnames = FALSE, 
          title = "Ofertas de Trabajo Estimada",
          dep.var.caption = "Ecuaci?n Estimada",
          dep.var.labels = "Horas Trabajadas")



#####
# Respuestas del Ejemplo 2
rm(list=ls())

datos <- wooldridge::bwght

#Modelo T
datos <- datos %>%
  mutate(peso = bwght/16)

mod <- lm(peso~cigs+faminc+cigtax , data = datos)


#Reportando los resultados
stargazer(mod, type = "text",
          colnames = FALSE, title = "Ecuaci?n Estimada",
          dep.var.caption = "Ecuaciones Estimadas",
          dep.var.labels = "Peso al Nacer (libras)")
