# Autor: Fidel Morla
# Objetivo: Mostrar capacidades de R para el análisis exploratorio
setwd("materias/Seminario de economía/organizar la data")
# ---------------------
# 1. Librerías necesarias
# ---------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(skimr)
library(GGally)
library(corrplot)

# ---------------------
# 2. Cargar y preparar los datos
# ---------------------
data <- read_excel("data.xlsx", sheet = "vars") %>%
  clean_names() |> 
  filter(year < 2025)

# ---------------------
# 3. Revisión general
# ---------------------
glimpse(data)
skim(data)

# ---------------------
# 4. Valores faltantes
# ---------------------
colSums(is.na(data))

# ---------------------
# 5. Estadísticas descriptivas personalizadas
# ---------------------
summary_stats <- data %>%
  select(x1, x2, x3, x4) %>%
  summarise(
    across(everything(), list(
      media    = \(x) mean(x, na.rm = TRUE),
      mediana  = \(x) median(x, na.rm = TRUE),
      desv_std = \(x) sd(x, na.rm = TRUE),
      min      = \(x) min(x, na.rm = TRUE),
      max      = \(x) max(x, na.rm = TRUE)
    ))
  )

# summary_stats |> 
#   tidyr::pivot_longer(
#     cols = dplyr::everything(),
#     names_to = "medida", 
#     values_to = "value"
#   ) |> 
#   mutate(
#     medida
#   )

print(summary_stats)

# ---------------------
# 6. Histogramas
# ---------------------
data %>%
  select(x1, x2, x3, x4) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de variables")

# ---------------------
# 7. Boxplots (outliers)
# ---------------------
# Gráfico individual para cada variable

boxplot_vars <- c("x1", "x2", "x3", "x4")

for (var in boxplot_vars) {
  print(
    ggplot(data, aes_string(x = "''", y = var)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Boxplot de", var), y = var, x = "") +
      theme_minimal()
  )
}

# ---------------------
# 8. Tendencias en el tiempo
# ---------------------
ggplot(data, aes(x = year, y = x1)) +
  geom_line(color = "steelblue") +
  labs(title = "Evolución de x1", x = "Fecha", y = "x1") +
  theme_minimal()

# ---------------------
# 9. Transformaciones y relaciones no lineales
# ---------------------
ggplot(data, aes(x = log(x1), y = x4)) +
  geom_point() +
  geom_smooth()+        
  labs(title = "Relación entre log(x1) y x4") +
  theme_minimal()

# ---------------------
# 10. Tasa de crecimiento
# ---------------------
data <- data %>%
  arrange(year) %>%
  mutate(g_x1 = (x2 / lag(x2)) - 1)

ggplot(data, aes(x = year, y = g_x1)) +
  geom_line() +
  labs(title = "Tasa de crecimiento anual de x2")

# ---------------------
# 11. Matriz de correlación
# ---------------------
cor_matrix <- data %>%
  select(x1, x2, x3, x4) %>%
  cor(use = "complete.obs") %>%
  round(4)

print(cor_matrix)

# ---------------------
# 12. Mapa de calor de correlaciones
# ---------------------
corrplot(cor_matrix, method = "circle", type = "lower", 
         tl.col = "black", addCoef.col = "black", number.digits = 2)

# ---------------------
# 13. Dispersión individual y con ajuste lineal
# ---------------------
ggplot(data, aes(x = x2, y = x3)) +
  geom_point(color = "tomato") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relación entre x2 y x3", x = "x2", y = "x3") +
  theme_minimal()

# ---------------------
# 14. Gráfico de pares
# ---------------------
data %>%
  select(x1, x2, x3, x4) %>%
  ggpairs(title = "Relaciones entre variables")
