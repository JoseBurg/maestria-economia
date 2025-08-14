library(dplyr)
library(plm)
library(wooldridge)
library(stargazer)


# Parte II:
# Ejercicio 1: Efectos fijos y aleatorios ---------------------------------

# Modelo de efectos fijos.----
modelo1 <- plm(
  lwage ~ educ + exper + black + married + union,
  data = wagepan,
  model= "between"
)

modelo11 <- plm(
  lwage ~ educ + exper + black + married + union,
  data = wagepan,
  model= "within"
)

stargazer(
  modelo1,
  modelo11,
  type = "text"
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
phtest(modelo11, modelo12)

# Preguntas:  -------------------------------------------------------------
# (c) Realice la prueba de Hausman y determine qu´e modelo es m´as apropiado.


# La prueba de Hausman contrasta la hipótesis nula de que el estimador de efectos aleatorios (EA) es consistente y eficiente contra la alternativa de que solo el estimador de efectos fijos (EF) es consistente.

# En tu caso, el estadístico de Hausman para comparar EF y EA es:
#   
# Con modelo "between": χ²(5) = 21.801, p = 0.0005711
# 
# Con modelo "within": χ²(3) = 22.816, p = 0.00004412
# 
# En ambos casos, p < 0.01, por lo que se rechaza la hipótesis nula. Esto indica que el modelo de efectos aleatorios es inconsistente y, por tanto, el modelo de efectos fijos es el más apropiado para este conjunto de datos.


# (d) Interprete el efecto de pertenecer a un sindicato (union).

# En el modelo de efectos fijos (within), el coeficiente estimado para union es 0.084 y es estadísticamente significativo al 1% (p < 0.01).
# 
# Interpretación: Manteniendo constantes el resto de las variables y considerando solo la variación dentro del individuo en el tiempo, pertenecer a un sindicato se asocia, en promedio, con un incremento del 8.4% en el salario por hora real (como la variable dependiente está en logaritmos, este coeficiente puede interpretarse aproximadamente como un cambio porcentual).
# 
# Esto sugiere que afiliarse a un sindicato está positivamente relacionado con mayores salarios, posiblemente debido a la negociación colectiva y mejores condiciones laborales que logran estas organizaciones.


# Ejercicio 2: Cambios en el crimen ---------------------------------------
modelo21 <- plm(
  crmrte ~ prbarr + prbconv + avgsen + polpc + density + income,
  data = crime4,
  model= "between"
)


modelo11 <- plm(
  lwage ~ educ + exper + black + married + union,
  data = wagepan,
  model= "within"
)



