library(wooldridge)
library(lmtest)
library(nlme)
library(sandwich)


# Estimar el modelo lineal:
# ln(wqage) = b0 + b1*educ + b2*exper + b3*exper^2 + u

model <- lm(log(wage) ~ educ + exper + I(exper^2), data = wage1)

summary(model)

# Gráficar los residuos contra los valores ajustados
plot(model$fitted.values, model$residuals,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Valores ajustados")
# Se puede observar heterocedasticidad en los residuos

# Realizar la prueba de Breusch-Pagan
bptest(model)

# La hipótesis nula de homocedasticidad es rechazada (p-valor < 0.05) 
# El valor del p-value es igual a 0.0001832
# Se concluye que hay heterocedasticidad en los residuos del modelo

# Correción de la heterocedasticidad --------------------------------------
# Existen dos maneras de corregir la heterocedasticidad:

# De la primera forma, se pueden usar errores estándar robustos

# Modelo  gls:
gls_model <- gls(log(wage) ~ educ + exper + I(exper^2), data = wage1,
                 weights = varIdent(form = ~educ))


# Explicación de la función gls y varIdent:
# La función gls (generalized least squares) se utiliza para ajustar modelos de regresión
# cuando los supuestos de homocedasticidad y no autocorrelación de los errores no se cumplen. 
# La función varIdent permite especificar una estructura de varianza diferente para diferentes grupos
# en los datos. En este caso, se está modelando la varianza de los errores como una función de la variable educ.


summary(gls_model)
plot(gls_model$fitted, gls_model$residuals,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Valores ajustados (GLS)")

# Correción de la matriz de varianza-covarianza ----
# La segunda forma de corregir la heterocedasticidad es usar la matriz de varianza-covarianza robusta
# Usando la función vcovHC del paquete sandwich
coeftest(model, vcov = vcovHC(model, type = "HC1"))


# ---------------------------------------------------------------------------------------------
# Minimos cuadrados generalizafos factibles -------------------------------

# Paso 1: Estimar el modelo por MCO y obtener los residuos
model_mco <- lm(log(wage) ~ educ + exper + I(exper^2), data = wage1)
residuals_mco <- resid(model_mco)

# Función::

mcgf <- function(
  formula, 
  data, 
  max_iter = 10, 
  tolerancia = 1e-5  
){
  
  for (i in 1:max_iter) {
    log_residuals_sq <- log(residuals_mco^2)
    aux <- lm(log_residuals_sq ~ fitted(model_mco))
    pesos = 1/exp(fitted(aux))
    data$pesos = pesos
    modelo_mcgf <- lm(log(wage) ~ educ + exper + I(exper^2), data = data, weights = pesos)
    new_residuals_mco <- resid(modelo_mcgf)
    
    if (max(new_residuals_mco - residuals_mco) < tolerancia) {"Convergio"
    break
    }
    residuals_mco <- new_residuals_mco
    modelo_mco <- modelo_mcgf
  }
  return(modelo_mcgf)
}
# Aplicación de la función mcgf
modelo <- mcgf(formula = log(wage) ~ educ + exper + I(exper^2), data = wage1)

stargazer::stargazer(modelo, type = "text")

#   Explicación del código: -----------------------------------
# La función mcgf implementa el procedimiento de mínimos cuadrados generalizados factibles (MCGF)
# para corregir la heterocedasticidad en un modelo de regresión lineal.
# La función toma como argumentos:
# - formula: la fórmula del modelo de regresión
# - data: el conjunto de datos
# - max_iter: el número máximo de iteraciones permitidas (por defecto 10)
# - tolerancia: el criterio de convergencia (por defecto 1e-5)
# La función realiza los siguientes pasos:
# 1. Estima el modelo por MCO y obtiene los residuos.
# 2. En cada iteración, estima la varianza condicional de los errores como una función lineal de las variables explicativas.
# 3. Calcula los pesos como el inverso de la varianza condicional.
# 4. Estima el modelo por MCO ponderado utilizando los pesos calculados.
# 5. Verifica si los residuos han convergido según el criterio de tolerancia.
# 6. Si ha convergido, sale del bucle; si no, actualiza los residuos y repite el proceso.
# Finalmente, la función devuelve el modelo estimado por MCGF.
# Fin de explicación del código -------------------------------------------------------------


# Paso 2: Estimar la varianza condicional de los errores
# Se asume que la varianza condicional de los errores es una función lineal de las variables explicativas
# Formula: sigma^2 = a0 + a1*educ + a2*exper + a3*exper^2

model_var <- lm(I(residuals_mco^2) ~ educ + exper + I(exper^2), data = wage1)
summary(model_var)
# Obtener los valores ajustados de la varianza condicional
fitted_var <- fitted(model_var)
# Asegurarse de que los valores ajustados sean positivos
fitted_var <- ifelse(fitted_var < 0, 0, fitted_var)
# Calcular los pesos como el inverso de la varianza condicional
weights <- 1 / fitted_var
# Paso 3: Estimar el modelo por MCO ponderado
model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2), data = wage1, weights = weights)
summary(model_fgls)
# Comparar los resultados con el modelo original
summary(model_mco)
summary(model_fgls)
