# Práctica 3 - Tarea : Modelos de Variable Dependiente Cualitativa o Limitada

# ------------------------------------------------------------
# Ejercicio 1 : Modelo de Respuesta Binaria (Probit o Logit)
# ------------------------------------------------------------

#Limpiar ambiente
rm(list = ls(all.names = TRUE))

# Limpiar consola
cat("\014")

# Paquetes necesarios
libs <- c("tidyverse","margins","AER")
lapply(libs, require, character.only = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(skimr)
library(GGally)
library(corrplot)
library(dplyr)
library(ggplot2)
library(stargazer)
library(gridExtra)

# 1) Cargar datos
data.attend <- read_csv("attend.csv")

# 2) Modelo Probit
mod_probit <- glm(attend ~ faminc + parcoll + urban + income, 
                  family = binomial(link = "probit"), 
                  data = data.attend)

stargazer(mod_probit,
          type = "html",
          title = "Modelo Probit: Asistencia a la universidad",
          dep.var.labels = "attend (1 = asistió)",
          covariate.labels = c("Ingreso familiar (faminc)",
                               "Padres con universidad (parcoll)",
                               "Urbano (urban)",
                               "Ingreso individual (income)"),
          digits = 4, no.space = TRUE,
          out = "probit_attend.html")

# 3) Efectos marginales promedio (AME)
mfx <- margins(mod_probit)
summary(mfx)

# 4) Efectos marginales solo para faminc y parcoll
mfx_vars <- margins(mod_probit, variables = c("faminc", "parcoll"))
summary(mfx_vars)

# Presentar resultados

sum_mfx <- summary(mfx)

tabla_ame <- sum_mfx |>
  transmute(Variable = factor,
            AME = round(AME, 4),
            `EE (SE)` = round(SE, 4),
            z = round(z, 3),
            `p-valor` = signif(p, 3),
            `IC 95% inferior` = round(lower, 4),
            `IC 95% superior` = round(upper, 4))

tabla_ame$Variable <- dplyr::recode(tabla_ame$Variable,
                                    faminc = "Ingreso familiar (faminc)",
                                    parcoll = "Padres con universidad (parcoll)",
                                    urban  = "Urbano (urban)",
                                    income = "Ingreso individual (income)")

stargazer(tabla_ame,
          type = "html",
          summary = FALSE,
          title = "Efectos marginales promedio (AME) - Probit attend",
          digits = 4,
          out = "probit_attend_margins.html")


# 5) Predicción de probabilidades
data.attend$pred_prob <- predict(mod_probit, type = "response")
head(data.attend$pred_prob)

# 6) Verificar Ajuste de predicción

pct_real <- mean(data.attend$attend) * 100

data.attend <- data.attend %>%
  mutate(pred_prob = predict(mod_probit, newdata = data.attend, type = "response"),
         pred_class = ifelse(pred_prob >= 0.5, 1, 0))

pct_predicho <- mean(data.attend$pred_class) * 100

cat("Porcentaje real de asistencia:", round(pct_real, 1), "%\n")
cat("Porcentaje predicho como asistencia:", round(pct_predicho, 1), "%\n")

# ------------------------------------------------------------
# Ejercicio 2 : Modelo Logit Multinomial
# ------------------------------------------------------------
library(nnet)

# 1) Cargar datos (travelmode está en AER)
data("TravelMode", package = "AER")
data.travelmode <- TravelMode

# 2) Asegurar que la variable dependiente es factor y base = "car"

data.travelmode$mode <- factor(data.travelmode$mode)
data.travelmode$mode <- relevel(data.travelmode$mode, ref = "car")

# 3) Ajustar el modelo logit multinomial
mod_logit_mnl <- multinom(mode ~ income + wait + gcost,
                          data = data.travelmode, trace = FALSE)

# Niveles actuales de la variable mode
levels(data.travelmode$mode)
table(data.travelmode$mode, useNA = "ifany")

# 4) Resultados en stargazer
summary(mod_logit_mnl)

stargazer(mod_logit_mnl,
          type = "html",
          title = "Modelo Logit Multinomial: Elección de modo de transporte",
          dep.var.caption = "Categoría base: automóvil (car)",
          digits = 4, no.space = TRUE,
          column.labels = c("Air", "Train", "Bus"),
          out = "mod_logit_mnl.html")

# Predicción para individuo con ingreso medio

inc_mean <- mean(data.travelmode$income, na.rm = TRUE)
newdata  <- data.travelmode
newdata$income <- inc_mean

# 2) Predecir probabilidades para cada observación con income fijado
probs_mat <- predict(mod_logit_mnl, newdata = newdata, type = "probs")

# 3) Promedio de probabilidades por alternativa (individuo con ingreso medio)
prob_med_income <- colMeans(as.matrix(probs_mat))

# 4) Tabla ordenada en %
tabla_prob_med_income <- tibble(
  mode = names(prob_med_income),
  prob = as.numeric(prob_med_income)
) |>
  arrange(desc(prob)) |>
  mutate(pct = round(100 * prob, 1))

# redondeo a enteros

tabla_prob_med_income <- tabla_prob_med_income %>%
  mutate(pct = round(pct, 0))  

#Visualización

ggplot(tabla_prob_med_income, aes(x = reorder(mode, -pct), y = pct, fill = mode)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 4) +
  labs(title = "Probabilidades predichas para individuo con ingreso medio",
       x = "Modo de transporte",
       y = "Probabilidad (%)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# ------------------------------------------------------------
# Ejercicio 3 : Modelo Logit Ordenado
# ------------------------------------------------------------

library(MASS)
library(haven)

# 1. Cargar data

data.jobsat <- read.csv("jobsat.csv")

# 2.  Convertir satisfact en factor ordenado
data.jobsat$satisfact <- factor(
  data.jobsat$satisfact,
  levels = c(1, 2, 3),
  ordered = TRUE,
  labels = c("Baja","Media","Alta")
)

if (!is.factor(data.jobsat$union)) {
  data.jobsat$union <- factor(data.jobsat$union, levels = c(0,1))
} else {
  # por seguridad, si los niveles no son 0/1, los dejamos como están y los reutilizamos abajo
  data.jobsat$union <- droplevels(data.jobsat$union)
}

# 3. Estimar logit ordenado

mod_logit_ordenado <- polr(satisfact ~ age + educ + tenure + union,
                   data = data.jobsat, 
                   method = "logistic", 
                   Hess = TRUE)

summary(mod_logit_ordenado)

# 4. Presentación

stargazer(mod_logit_ordenado, type = "html",
          title = "Logit ordenado: satisfacción laboral",
          dep.var.labels = "satisfact (ordenada)", digits = 4, no.space = TRUE,
          out = "mod_logit_ordenado.html")

# 5. Efecto de UNION: odds ratio e IC
OR_union <- exp(coef(mod_logit_ordenado)["union1"])  # si tu factor tiene niveles 0/1, el contraste es 1 vs 0
OR_union

# 7. Probabilidades predichas por NIVEL DE EDUCACIÓN ----------------------

nivel_base_union <- levels(data.jobsat$union)[1]

grid_educ <- data.frame(
  age    = mean(data.jobsat$age, na.rm = TRUE),
  tenure = mean(data.jobsat$tenure, na.rm = TRUE),
  union  = factor(nivel_base_union, levels = levels(data.jobsat$union)),
  educ   = seq(min(data.jobsat$educ, na.rm = TRUE),
               max(data.jobsat$educ, na.rm = TRUE), by = 1)
)

pred_probs <- predict(mod_logit_ordenado, newdata = grid_educ, type = "probs")
pred_probs

summary(pred_probs)

# Presentar gráficamente

pred_probs_df <- cbind(grid_educ, pred_probs)

pred_probs_long <- pred_probs_df %>%
  pivot_longer(cols = c("Baja", "Media", "Alta"),
               names_to = "Satisfaccion",
               values_to = "Probabilidad")

ggplot(pred_probs_long, aes(x = educ, y = Probabilidad, color = Satisfaccion)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "blue", "darkgreen")) +
  labs(title = "Probabilidades Predichas por Nivel de Educación",
       x = "Años de Educación",
       y = "Probabilidad Predicha",
       color = "Nivel de Satisfacción") +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------
# Ejercicio 4 : Modelo Tobit (Censura)
# ------------------------------------------------------------

library(AER)    
library(haven)    
library(margins)   
library(dplyr)
library(censReg)
install.packages("censReg")

# 1. Cargar data
data.healthexp <- read_csv("healthexp.csv")

# 2. Estimar modelo Tobit

mod_tobit <- tobit(exph ~ age + income + insured + chronic,
                   left = 0, data = data.healthexp)


stargazer(mod_tobit, type = "text",
          title = "Modelo Tobit: gasto en salud (exph)",
          dep.var.labels = "exph", digits = 4, no.space = TRUE)

# 3. Comparar con OLS

mod_ols_tobit<- lm(exph ~ age + income + insured + chronic,
              data = data.healthexp)

# 4. Comparación stargazer

stargazer(mod_tobit, mod_ols_tobit,
          type = "html",
          title = "Comparación de modelos: Tobit vs OLS",
          dep.var.labels = "Gasto médico anual (exph)",
          column.labels = c("Tobit censurado", "OLS"),
          digits = 3,
          align = TRUE,
          no.space = TRUE,
          omit.stat = c("ll","ser","f"),
          add.lines = list(
            c("N", nobs(mod_tobit), nobs(mod_ols))
          ),
          notes = "Errores estándar entre paréntesis. *p<0.1; **p<0.05; ***p<0.01",
          out = "mod_tobit_ols.html")

# 4. Calcular efecto marginal esperado del ingreso

# Objetos básicos
beta  <- coef(mod_tobit)
sigma <- summary(mod_tobit)$scale
X     <- model.matrix(mod_tobit)
mu    <- as.numeric(X %*% beta)      # xβ
z     <- mu / sigma

Phi <- pnorm(z); phi <- dnorm(z)

# Media incondicional predicha E[y|x]
Ey  <- Phi * mu + sigma * phi

## 1) AME 
b_inc   <- beta["income"]
ame_inc <- mean(Phi * b_inc)
ame_inc  # cambio esperado en exph por 1 unidad de income


# ------------------------------------------------------------
# Ejercicio 5 : Modelo de Selección Muestral (Heckman)
# ------------------------------------------------------------

library(sampleSelection)

data("mroz", package = "wooldridge")
data.mroz <- mroz

# 1. Estimar heckman 2 etapas 

mod_heckman <- heckit(
  selection = inlf ~ age + kidslt6 + kidsge6 + educ + nwifeinc,
  outcome   = lwage ~ educ + exper + I(exper^2) + city,
  data = mroz,
  method = "2step"
)

summary(mod_heckman)

# 2. Estimar OLS (solo si participa: inlf = 1)

mod_ols <- lm(lwage ~ educ + exper + I(exper^2) + city,
              data = subset(mroz, inlf == 1))

summary(mod_ols)

# 3. Comparación stargazer

stargazer(
  mod_heckman, mod_ols,
  type = "html",  
  title = "Comparación: Heckman (2 etapas) vs OLS directo",
  column.labels = c("Heckman - Salario", "OLS directo"),
  keep = c("educ", "exper", "I\\(exper\\^2\\)", "city"),
  header = FALSE,
  out = "heckman_old_mod.html"
)

# ------------------------------------------------------------
# Ejercicio 6 : Modelo de Conteo (Poisson / Binomial Negativa)
# ------------------------------------------------------------

# 1. Cargar librerías necesarias

library(tidyverse)
library(margins)
library(stargazer)

# 2. Cargar la base de datos

data.health <- read.csv("health.csv")

# 3. Modelo Poisson
mod_pois <- glm(numvisit ~ age + income + educ + insured + health,
                family = poisson(link = "log"),
                data = data.health)
# Presentación

stargazer(mod_pois, type = "html", 
          title = "Modelo Poisson - Visitas al médico",
          out = "mod_pois.html")

# 4. Efecto marginal de insured
marg_insured <- margins(mod_pois, variables = "insured")
summary(marg_insured)
