# ============================================
# EJERCICIO 4 - Análisis de regresiones con dos muestras
# ============================================

install.packages("matlib")      
install.packages("car")         
install.packages("lmtest")     

library(matlib)
library(car)
library(lmtest)

# Datos del ejercicio
# Matrices de momentos para muestra 1 y muestra 2

# X'X matrices
XtX1 <- matrix(c(50, 300, 300, 2100), nrow = 2, byrow = FALSE)
XtX2 <- matrix(c(50, 300, 300, 2100), nrow = 2, byrow = FALSE)

# X'y vectores
Xty1 <- matrix(c(300, 2000), nrow = 2)
Xty2 <- matrix(c(300, 2200), nrow = 2)

# y'y escalares
yty1 <- 2100
yty2 <- 2800

# ---------- (a) Estimación OLS por muestra ----------
# β_hat = (X'X)^(-1) X'y
beta1 <- solve(XtX1) %*% Xty1
beta2 <- solve(XtX2) %*% Xty2


# Cálculo de residuos: e'e = y'y - β'X'y
e1_sq <- yty1 - t(beta1) %*% Xty1
e2_sq <- yty2 - t(beta2) %*% Xty2

# Varianza residual s^2 = e'e / (n - k)
n <- 50
k <- 2
s2_1 <- e1_sq / (n - k)
s2_2 <- e2_sq / (n - k)

# R^2 = 1 - (e'e / TSS)
TSS1 <- yty1 - (sum(Xty1)^2 / sum(XtX1))
TSS2 <- yty2 - (sum(Xty2)^2 / sum(XtX2))
R2_1 <- 1 - as.numeric(e1_sq / TSS1)
R2_2 <- 1 - as.numeric(e2_sq / TSS2)

# ---------- (b) Estimación conjunta de β bajo varianza común ----------
# β_hat = (XtX1 + XtX2)^(-1) %*% (Xty1 + Xty2)
XtX_sum <- XtX1 + XtX2
Xty_sum <- Xty1 + Xty2
beta_pool <- solve(XtX_sum) %*% Xty_sum

# Error residual conjunto
e_pool_sq <- (yty1 + yty2) - t(beta_pool) %*% (Xty1 + Xty2)
s2_pool <- e_pool_sq / (2 * n - 2 * k)

# Matriz de covarianza asintótica
cov_beta_pool <- s2_pool[1, 1] * solve(XtX_sum)

# ---------- (c) Test de igualdad de varianzas: F-Test ----------
F_stat <- as.numeric(s2_1 / s2_2)
gl1 <- n - k
gl2 <- n - k
p_value_f <- 2 * min(pf(F_stat, gl1, gl2), 1 - pf(F_stat, gl1, gl2))

# ---------- (d) Estimador FGLS con varianza común pero diferente media ----------
# MCG = (X'Ω^(-1)X)^(-1) X'Ω^(-1)y — usando aquí pesos 1/s2 por muestra
Omega_inv1 <- diag(1 / s2_1[1, 1], n)
Omega_inv2 <- diag(1 / s2_2[1, 1], n)

# Como no tenemos los datos individuales, estimación MCG a mano no es posible.
# Pero podemos computar el estimador ponderado de varianza combinado:
w1 <- 1 / s2_1
w2 <- 1 / s2_2

# Convertir pesos a escalares numéricos
w1 <- as.numeric(1 / s2_1)
w2 <- as.numeric(1 / s2_2)

# Estimador FGLS corregido
beta_fgls <- solve(w1 * XtX1 + w2 * XtX2) %*% (w1 * Xty1 + w2 * Xty2)
cov_fgls <- solve(w1 * XtX1 + w2 * XtX2)

beta_fgls <- solve(w1 * XtX1 + w2 * XtX2) %*% (w1 * Xty1 + w2 * Xty2)
cov_fgls <- solve(w1 * XtX1 + w2 * XtX2)

# ---------- Resultados ----------
cat("Estimaciones OLS individuales:\n")
print(beta1); print(beta2)
cat("Varianzas residuales s2:\n")
print(s2_1); print(s2_2)
cat("R^2 por muestra:\n")
print(R2_1); print(R2_2)

cat("\nEstimación OLS combinada (b):\n")
print(beta_pool)
cat("Matriz de covarianza combinada:\n")
print(cov_beta_pool)

cat("\nF-Statistic para prueba de igualdad de varianzas:\n")
print(F_stat)
cat("P-valor (dos colas):\n")
print(p_value_f)

cat("\nEstimación FGLS (d):\n")
print(beta_fgls)
cat("Matriz de varianza FGLS:\n")
print(cov_fgls)

# ======================================
# EJERCICIO 8 
# Análisis de heterocedasticidad según el sexo
# ======================================

# install.packages("wooldridge")
# install.packages("dplyr")

library(wooldridge)
library(dplyr)

# -----------------------------
# 2. Cargar datos
# -----------------------------
data("sleep75")
df <- sleep75

# -----------------------------
# 3. Estimar modelo de MCO
# -----------------------------
modelo <- lm(sleep ~ totwrk + educ + age + yngkid + male, data = df)
summary(modelo)

# -----------------------------
# 4. Calcular residuos
# -----------------------------
df$residuals <- resid(modelo)

# -----------------------------
# 5. Comparar varianzas de residuos por sexo
# -----------------------------
# Calcular residuos y agregarlos al data frame
df$residuals <- resid(modelo)

# Comparar varianzas por sexo
varianzas <- df %>%
  group_by(male) %>%
  summarise(
    n = n(),
    var_residuos = var(residuals),
    sd_residuos = sd(residuals)
  )

print("Varianzas de los residuos por sexo:")
print(varianzas)


print(" Varianzas de los residuos por sexo:")
print(varianzas)

# -----------------------------
# 6. Prueba F para igualdad de varianzas
# -----------------------------
res_hombres <- df$residuals[df$male == 1]
res_mujeres <- df$residuals[df$male == 0]

f_test <- var.test(res_hombres, res_mujeres)

print("Prueba F de igualdad de varianzas:")
print(f_test)

# -----------------------------
# 7. Conclusión general
# -----------------------------
cat("\n--- Conclusión test de igualdad de varianzas ---\n")
cat(sprintf("F(%0.f, %0.f) = %.3f, p-valor = %.5f\n",
            f_test$parameter[1], f_test$parameter[2],
            f_test$statistic, f_test$p.value))

if (f_test$p.value < 0.05) {
  cat("→ Se RECHAZA H0: las varianzas difieren entre hombres y mujeres.\n")
  if (var_hombres > var_mujeres) {
    cat("   La varianza de los HOMBRES es mayor.\n")
  } else if (var_hombres < var_mujeres) {
    cat("   La varianza de las MUJERES es mayor.\n")
  } else {
    cat("   Las varianzas son prácticamente iguales numéricamente.\n")
  }
} else {
  cat("→ No se rechaza H0: no hay evidencia suficiente de heterocedasticidad por sexo.\n")
}

# ======================================
# EJERCICIO 9 
# Análisis de heterocedasticidad en precios de viviendas
# ======================================

# -----------------------------
# 1. Instalar y cargar librerías
# -----------------------------
# install.packages("wooldridge")
# install.packages("lmtest")
# install.packages("sandwich")

library(wooldridge)
library(lmtest)
library(sandwich)

# -----------------------------
# 2. Cargar datos
# -----------------------------
data("hprice1")
df <- hprice1

# -----------------------------
# 3. Estimar modelo por MCO
# -----------------------------
modelo_mco <- lm(price ~ lotsize + sqrft + bdrms, data = df)
summary(modelo_mco)

# -----------------------------
# 4. Prueba de heterocedasticidad: Breusch-Pagan
# -----------------------------
bp_test <- bptest(modelo_mco)

cat("\n Resultado prueba de Breusch-Pagan:\n")
print(bp_test)

if (bp_test$p.value < 0.05) {
  cat("Se rechaza H0: existe evidencia de heterocedasticidad.\n")
} else {
  cat("No se rechaza H0: no hay evidencia fuerte de heterocedasticidad.\n")
}

# -----------------------------
# 5. Estimar errores estándar robustos (White)
# -----------------------------
coeftest(modelo_mco, vcov = vcovHC(modelo_mco, type = "HC1"))

# ======================================
# EJERCICIO 10
# Dataset  "Vote1"
# ======================================

# -----------------------------
# 1. Instalar y cargar librerías
# -----------------------------
#install.packages("wooldridge")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("stargazer")

library(wooldridge)   
library(lmtest)       
library(sandwich)     
library(stargazer)    

# -----------------------------
# 2. Cargar datos
# -----------------------------
data("vote1")
head(vote1)

# -----------------------------
# 3. Estimar modelo por MCO
# -----------------------------
modelo_10a <- lm(voteA ~ prtystrA + democA + log(expendA) + log(expendB), data = vote1)
summary(modelo_10a)

# -----------------------------
# 4. Residuales
# -----------------------------
residuos_10a <- resid(modelo_10a)

# -----------------------------
# 5. Re-regresión de los residuos sobre las variables explicativas
# -----------------------------
reg_residuos <- lm(residuos_10a ~ prtystrA + democA + log(expendA) + log(expendB), data = vote1)
summary(reg_residuos)

# -----------------------------
# 6. Prueba de heterocedasticidad: Breusch-Pagan
# -----------------------------
library(lmtest)
bptest(modelo_10a)

# -----------------------------
# 7. Prueba de heterocedasticidad: Prueba de White
# -----------------------------
vote1$lexpendA <- log(vote1$expendA)
vote1$lexpendB <- log(vote1$expendB)

modelo_white <- lm(voteA ~ prtystrA + democA + lexpendA + lexpendB, data = vote1)

bptest(modelo_white, ~ prtystrA + democA + lexpendA + lexpendB +
         I(prtystrA^2) + I(democA^2) + I(lexpendA^2) + I(lexpendB^2) +
         I(prtystrA*democA) + I(prtystrA*lexpendA) + I(prtystrA*lexpendB) +
         I(democA*lexpendA) + I(democA*lexpendB) + I(lexpendA*lexpendB),
       data = vote1)

# ======================================
# EJERCICIO 11
# Dataset "Wage2"
# ======================================

# -----------------------------
# 1. Cargar datos
# -----------------------------
data("wage2")
head(wage2)

# -----------------------------
# 2. Estimar modelo por MCO
# -----------------------------
modelo_11a <- lm(log(wage) ~ educ + exper + tenure + married + black + south + urban, data = wage2)
summary(modelo_11a)

# -----------------------------
# 3. Variables Cuadráticas
# -----------------------------
wage2$exper2 <- wage2$exper^2
wage2$tenure2 <- wage2$tenure^2

# -----------------------------
# 4. Modelo extendido
# -----------------------------
modelo_11b <- lm(log(wage) ~ educ + exper + exper2 + tenure + tenure2 + married + black + south + urban, data = wage2)
summary(modelo_11b)

# -----------------------------
# 5. Prueba de significancia conjunta (test F)
# -----------------------------
anova(modelo_11a, modelo_11b)


# -----------------------------
# 6. Modelo de Interacción
# -----------------------------
#Rendimiento a la educación depende de la raza.

wage2$educ_black <- wage2$educ * wage2$black
modelo_11c <- lm(log(wage) ~ educ + black + educ_black + exper + tenure + married + south + urban, data = wage2)
summary(modelo_11c)

#salarios difieran entre cuatro grupos:
#1.Casados afroamericanos
#2.Casados no afroamericanos
#3.Solteros afroamericanos
#4.Solteros no afroamericanos

wage2$black_married <- wage2$black * wage2$married
modelo_11d <- lm(log(wage) ~ educ + exper + tenure + married + black + black_married + south + urban, data = wage2)
summary(modelo_11d)

# ======================================
# EJERCICIO 14
# Dataset "Fertil2"
# ======================================

# -----------------------------
# 1. Cargar datos
# -----------------------------
data("fertil2")
head(fertil2)

# -----------------------------
# 2. Variables Cuadráticas
# -----------------------------
fertil2$age2 <- fertil2$age^2

# -----------------------------
# 3. Estimar modelo por MCO
# -----------------------------
modelo_14a <- lm(children ~ educ + age + age2, data = fertil2)
summary(modelo_14a)

# -----------------------------
# 4. Verificación de relevancia
# -----------------------------
relevancia <- lm(educ ~ frsthalf + age + age2, data = fertil2)
summary(relevancia)

# -----------------------------
# 5. Estimar el modelo por MC2E
# -----------------------------
install.packages("AER") 
library(AER)

modelo_iv_14c <- ivreg(children ~ educ + age + age2 | frsthalf + age + age2, data = fertil2)
summary(modelo_iv_14c)

# -----------------------------
# 6. Estimar modelo ampliado
# -----------------------------
modelo_14d_mco <- lm(children ~ educ + age + age2 + electric + tv + bicycle, data = fertil2)
summary(modelo_14d_mco)

modelo_14d_iv <- ivreg(children ~ educ + age + age2 + electric + tv + bicycle |
                         frsthalf + age + age2 + electric + tv + bicycle, data = fertil2)
summary(modelo_14d_iv)

# ======================================
# EJERCICIO 15
# Viñedos
# ======================================

# -----------------------------
# 1. Cargar datos
# -----------------------------
library(readr)
CHARD <- read_csv("Práctica 2/CHARD.csv")
View(CHARD)

# -----------------------------
# 2. Estimar modelo por MCO
# -----------------------------
modelo_15a <- lm(Q ~ XPER + CAP + LAB, data = CHARD)
summary(modelo_15a)

# -----------------------------
# 3.Intervalos de predicción para la producción de vino
# -----------------------------
# Calcular promedios de capital y trabajo
mean_cap <- mean(CHARD$CAP, na.rm = TRUE)
mean_lab <- mean(CHARD$LAB, na.rm = TRUE)

# Crear nuevo data frame con los 3 escenarios
nuevos_datos <- data.frame(
  XPER = c(10, 20, 30),
  CAP = rep(mean_cap, 3),
  LAB = rep(mean_lab, 3)
)

# Predecir con intervalos de confianza al 95%
predicciones <- predict(modelo_15a, newdata = nuevos_datos, interval 
                        = "confidence")
predicciones

# -----------------------------
# 4.Test de Hausman con residuos
# -----------------------------
reg_aux <- lm(XPER ~ AGE + CAP + LAB, data = CHARD)
residuos_hausman <- resid(reg_aux)

modelo_hausman <- lm(Q ~ XPER + CAP + LAB + residuos_hausman, data = CHARD)
summary(modelo_hausman)

# -----------------------------
# 5.Estimación por VI
# -----------------------------
install.packages("AER")  
library(AER)

modelo_15d_vi <- ivreg(Q ~ XPER + CAP + LAB | AGE + CAP + LAB, data = CHARD)
summary(modelo_15d_vi)

# -----------------------------
# 6.Intervalos de predicción con modelo VI
# -----------------------------
mean_cap <- mean(CHARD$CAP, na.rm = TRUE)
mean_lab <- mean(CHARD$LAB, na.rm = TRUE)

nuevos_datos_vi <- data.frame(
  XPER = c(10, 20, 30),
  CAP = rep(mean_cap, 3),
  LAB = rep(mean_lab, 3),
  AGE = c(35, 45, 55)  # Se puede poner una edad razonable para mantener consistencia
)

predicciones_vi <- predict(modelo_15d_vi, newdata = nuevos_datos_vi, 
                           interval = "confidence")
predicciones_vi
