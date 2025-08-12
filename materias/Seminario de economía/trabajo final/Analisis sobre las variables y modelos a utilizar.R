library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)
library(car)
library(AER)
library(stargazer)

pension <- readxl::read_excel(
  "materias/Seminario de economía/trabajo final/pension.xlsx"
)


# Distribución de la variable dependiente
ggplot(pension, aes(x = pctstck)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  labs(title = "Distribución del porcentaje invertido en acciones")

# Correlaciones entre variables numéricas
pension |>
  select(pctstck, educ, wealth89, age) |>
  ggpairs()

# Comparaciones por IRA y matrimonio
pension |>
  pivot_longer(
    cols = c(married, irain89),
    names_to = "variable",
    values_to = "valor"
  ) |>
  ggplot(aes(x = as.factor(valor), y = pctstck)) +
  geom_boxplot() +
  facet_wrap(~variable) +
  labs(y = "Porcentaje invertido en acciones", x = "")


# Modelo lineal básico
modelo_lineal <- lm(
  pctstck ~ educ + wealth89 + age + married + irain89,
  data = pension
)
summary(modelo_lineal)


# Residuos vs ajustados
plot(modelo_lineal, which = 1)

# Normalidad de residuos
plot(modelo_lineal, which = 2)

pension_mujeres <- pension |>
  mutate(pctstck_frac = pctstck / 100)

modelo_frac <- glm(
  pctstck_frac ~ educ + wealth89 + age + married + irain89,
  data = pension_mujeres,
  family = quasibinomial()
)
summary(modelo_frac)


# Multicolinealidad
vif(modelo_lineal)

modelo_tobit <- tobit(
  pctstck ~ educ + wealth89 + age + married + irain89,
  data = pension,
  left = 0,
  right = 100
)
summary(modelo_tobit)

stargazer(
  modelo_lineal,
  modelo_frac,
  modelo_tobit,
  type = "text",
  title = "Resultados de los modelos",
  column.labels = c("Lineal", "Frac", "Tobit"),
  digits = 3,
  out = "resultados_modelo.txt"
)

# Analisis PCA sobre las variables numéricas
pension_num <- pension |>
  select(educ, wealth89, age)
pca_result <- prcomp(pension_num, scale. = TRUE)
summary(pca_result)
biplot(pca_result, main = "Biplot del Análisis PCA")
# Agregar las componentes principales al dataframe original
pension <- pension |>
  mutate(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])
# Modelo con componentes principales
modelo_pca <- lm(pctstck ~ PC1 + PC2 + married + irain89, data = pension)
summary(modelo_pca)
stargazer(
  modelo_lineal,
  modelo_pca,
  type = "text",
  title = "Resultados de los modelos con y sin PCA",
  column.labels = c("Lineal", "PCA"),
  digits = 3,
  out = "resultados_modelo_pca.txt"
)
# Comparación de modelos
anova(modelo_lineal, modelo_pca)
# Gráficos de diagnóstico para el modelo con PCA
par(mfrow = c(2, 2))
plot(modelo_pca)
par(mfrow = c(1, 1))


# Analisis de modelos con paquete perfomance ------------------------------

library(performance)
check_model(modelo_lineal)
check_model(modelo_frac)
check_model(modelo_tobit)
check_model(modelo_pca)
# Comparación de modelos con AIC y BIC
modelos <- list(
  Lineal = modelo_lineal,
  Frac = modelo_frac,
  Tobit = modelo_tobit,
  PCA = modelo_pca
)
modelos_aic <- sapply(modelos, AIC)
modelos_bic <- sapply(modelos, BIC)
modelos_aic
modelos_bic
# Gráfico de comparación de AIC y BIC
aic_bic_df <- data.frame(
  Modelo = names(modelos_aic),
  AIC = modelos_aic,
  BIC = modelos_bic
)
ggplot(aic_bic_df, aes(x = Modelo)) +
  geom_bar(aes(y = AIC), stat = "identity", fill = "lightblue") +
  geom_bar(aes(y = BIC), stat = "identity", fill = "lightgreen", alpha = 0.7) +
  labs(
    title = "Comparación de AIC y BIC entre modelos",
    y = "Valor",
    x = "Modelo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Análisis limpio de los modelos y descripción de los datos  --------------
# Comportamiento de variables dependientes y explicativas
pension |>
  select(pctstck, educ, wealth89, age) |>
  summary() |>
  kableExtra::kable(
    caption = "Resumen de las variables dependientes y explicativas"
  )

## Gráficos de las variables dependientes y explicativas
pension |>
  select(pctstck, educ, wealth89, age) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribución de variables dependientes y explicativas") +
  theme_minimal()

# Análisis de correlación entre variables
correlacion <- cor(
  pension |> select(pctstck, educ, wealth89, age),
  use = "complete.obs"
)
correlacion |>
  as.data.frame() |>
  kableExtra::kable(
    caption = "Matriz de correlación entre variables dependientes y explicativas"
  )

# Gráfico de correlación
library(corrplot)
corrplot(
  correlacion,
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 45
)
