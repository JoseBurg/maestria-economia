#%%%%%% EJEMPLOS MODELO DE REGRESION LINEAL %%%%%%%%%%%%%%%%%
rm(list=ls())

#Paquetes a utilizar
library(dplyr)
library(wooldridge)
library(stargazer)
library(car)

#####
# Respuestas Ejemplo 1
datos_mod <- wooldridge::mroz

#a) Estimando los modelos
mod_salario <- lm(hours ~ wage+educ+kidslt6+kidsge6, data= datos_mod)


#Reportando los resultados
stargazer(mod_salario, type = "text",
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


# Prubeas de hipotesis ----------------------------------------------------
linearHypothesis(
  mod_salario, 
  c("wage=0")  # Hipotesis nula
)              # H0: beta = 0

# Test de hipotesis F
qf(0.05, 1, 423, lower.tail = FALSE)



# Ejemplo siguiente:
linearHypothesis(
  mod_salario, 
  c("kidslt6-kidsge6=0")  # Hipotesis nula
)                         # H0: kidslt6 - kidsge6 = 0

# Test de hipotesis F: F tabla
qf(0.05, 1, 423, lower.tail = FALSE)

