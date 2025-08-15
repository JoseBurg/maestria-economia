library(dplyr)
library(plm)
library(wooldridge)
library(stargazer)


# Parte II:
# Ejercicio 1: Efectos fijos y aleatorios ---------------------------------

# Modelo de efectos fijos.----
# Convertir la data de panel

wagepan <- pdata.frame(
  wagepan, # ID     t
  index = c("nr", "year")
)


modelo1 <- plm(
  lwage ~ educ + exper + black + married + union,
  data = wagepan,
  model= "between"
)

# Modelo de efectos aleatorios -----
modelo12 <- plm(
  lwage ~ educ + exper + black + married + union,
  data = wagepan,
  model = "random"
)

stargazer(
  modelo12,
  type = "text"
)

## Test de Hausman:

phtest(modelo1, modelo12)

# Preguntas:  -------------------------------------------------------------
# (c) Realice la prueba de Hausman y determine qu´e modelo es m´as apropiado.


# (d) Interprete el efecto de pertenecer a un sindicato (union).

stargazer(
  modelo1,
  type = "text"
)

# Ejercicio 2: Cambios en el crimen ---------------------------------------
# NOTA:     La variable dependiente e income no estan en la misma data

# Estime un modelo de efectos fijos para evaluar el impacto
# de la severidad del castigo sobre la criminalidad -----------
crimen <- pdata.frame(
  crime4, # ID     t
  index = c("county", "year")
)

modelo21 <- plm(
  crmrte ~ prbarr + prbconv + avgsen + polpc + density,
  data = crimen,
  model= "between"
)

stargazer(
  modelo21,
  type = "text",
  title = "Modelo de Efectos Fijos para el Crimen"
)

# ¿Cómo cambia el coeficiente de prbarr se se omite polc? ----

modelo22 <- plm(
  crmrte ~ prbarr + prbconv + avgsen + density,
  data = crimen,
  model= "between"
)

stargazer(
  modelo21, modelo22,
  type = "text",
  title = "Comparación de Modelos con y sin polpc"
)


# Incluya efectos temporales fijos (años) y analice si la tendencia de
# criminalidad cambia.
# Estimación del modelo con efectos temporales fijos
modelo23 <- plm(
  crmrte ~ prbarr + prbconv + avgsen + density,
  data = crimen,
  model= "within",
  efect = "twoways"
)

stargazer(
  modelo21, modelo23,
  type = "text",
  title = "Comparación de Modelos con y sin Efectos Temporales"
)


# Ejercicio 3: Productividad agricola -------------------------------------
## Ejercicio 3: Productividad agrícola

# Variable dependiente: `crop output`
# 
# Variables explicativas: `fertilizer`, `labor`, `land`

### (a) Estime un modelo de efectos fijos para evaluar el impacto del fertilizante.


fertil <- read.csv("./fertilizer.csv")
# Convertir a datos de panel, controlando por country y year
fertil <- pdata.frame(fertil, index = c("country", "year"))
modelo3 <- plm(crop_output ~ fertilizer + labor + land,
               data = fertil, model = "within")
stargazer(modelo3, type = "text")

### (b) Incluya el término cuadrático de fertilizer y comente si hay rendimientos decrecientes.

modelo31 <- plm(crop_output ~ fertilizer + I(fertilizer^2) + labor + land,
                data = fertil, model = "within")
stargazer(modelo31, type = "text")

### (c) Estime un modelo con efectos aleatorios y compare con el modelo anterior.

modelo32 <- plm(crop_output ~ fertilizer + I(fertilizer^2) + labor + land,
                data = fertil, model = "random")
stargazer(modelo31, modelo32, type = "text",
          title = "Comparación de Modelos de Efectos Fijos y Aleatorios")

phtest(modelo31, modelo32)