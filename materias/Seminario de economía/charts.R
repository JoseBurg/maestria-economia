# Cargar librerías necesarias
library(readxl)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(tidyverse)
library(scales)
# Opcional para matrices de dispersión

# Dispersion ---------------------------
# Importar datos
data_disp <- read_excel("data.xlsx", sheet = "disp")
data_disp <- read_excel("materias/Seminario de economía/data.xlsx", sheet = "vars") |> 
  filter(year < 2025)

# 1. Gráfico de dispersión x1 vs x2

data_disp |> 
  ggplot(aes(x = x1, y = x2)) +
    geom_smooth(method = "lm", se = FALSE, color = "red4",
                linetype = 'dashed') +
    geom_point(color = "steelblue4", size = 2.5,
               fill = "steelblue1",
               shape = 21) +
    labs(title = "Dispersión: x1 vs x2",
         x = "x1", y = "x2") +
    theme_stata()

# 2. Gráfico de dispersión x1 vs x3 con línea de tendencia
ggplot(data_disp, aes(x = x1, y = x3)) +
  geom_point(color = "tomato4", size = 2.5,
             fill = "tomato1",
             shape = 21) +
  geom_smooth(method = "lm", se = FALSE, color = "gray11",
              linetype = 'dashed') +  
  labs(
    title = "Dispersión: x1 vs x3 con tendencia lineal",
    x = "x1",
    y = "x3"
  )  +
  theme_stata()

# 3. Gráfico de dispersión x2 vs x3 con línea de tendencia
ggplot(data_disp, aes(x = x2, y = x3)) +
  geom_point(color = "seagreen4", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "mediumpurple4",
              linetype = 'dashed') +  labs(
    title = "Dispersión: x2 vs x3 con tendencia lineal",
    x = "x2",
    y = "x3"
  )  +
  theme_stata()


# Lineas ------------------------------------------------------------------

# Importar datos de línea desde la hoja "lines"
# data_lines <- read_excel("data.xlsx", sheet = "lines")

# Convertir a formato largo para graficar varias series en un mismo plot
long_lines <- data_disp  |> 
  pivot_longer(cols = -year, names_to = "variable", values_to = "valor")

# 1.a Gráfico de líneas de todas las variables juntas
long_lines %>% 
  filter(variable %in% c('x1', 'x3')) %>% 
ggplot(aes(x = year, y = valor, color = variable)) +
  geom_line(size = 1) +
  labs(
    title = "Series temporales para sheet = lines",
    x = "Fecha",
    y = "Valor",
    color = "Variable"
  ) +
  theme_stata() +
  scale_color_stata() +
  scale_fill_stata() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 1.b Gráfico de líneas de todas las variables juntas
long_lines %>% 
  filter(variable %in% c('x2', 'x3', 'x1')) %>% 
  ggplot(aes(x = year, y = valor, color = variable)) +
  geom_line(size = 1) +
  labs(
    title = "Series temporales para sheet = lines",
    x = "Fecha",
    y = "Valor",
    color = "Variable"
  )   +
  scale_color_stata() +
  scale_fill_stata() +
  theme_stata() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 2. Gráficos de líneas individuales
# x1 over time
ggplot(data_disp, aes(x = year, y = x1)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Evolución de x1 en el tiempo",
    x = "Fecha",
    y = "x1"
  ) +
  scale_color_stata() +
  scale_fill_stata() +
  theme_stata() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# x2 over time
ggplot(data_disp, aes(x = year, y = x2)) +
  geom_line(color = "tomato", size = 1) +
  labs(
    title = "Evolución de x2 en el tiempo",
    x = "Fecha",
    y = "x2"
  ) +
  scale_color_stata() +
  scale_fill_stata() +
  theme_stata() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# x3 over time
ggplot(data_disp, aes(x = year, y = x3)) +
  geom_line(color = "forestgreen", size = 1) +
  labs(
    title = "Evolución de x3 en el tiempo",
    x = "Fecha",
    y = "x3"
  ) +
  scale_color_stata() +
  scale_fill_stata() +
  theme_stata() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Bars --------------------------------------------------------------------

# Importar datos
data_bar <- read_excel("data.xlsx", sheet = "bar")

# Transformar a formato largo para ciudades x2 (SD) y x3 (Santiago)
long_bar <- data_bar %>%
  pivot_longer(
    cols      = c(x2, x3),
    names_to  = "ciudad",
    values_to = "ventas"
  )

# Definir posición para evitar repetición
dodge <- position_dodge(width = 0.8)

# 1. Barras agrupadas: ventas por fruta y año, comparando ciudades
ggplot(long_bar, aes(x = factor(date), y = ventas, fill = ciudad)) +
  geom_col(position = dodge, width = 0.7) +
  facet_wrap(~ x1) +
  labs(
    title = "Ventas Anuales por Fruta y Ciudad",
    x     = "Año",
    y     = "Ventas",
    fill  = "Ciudad:"
  ) +
  scale_fill_stata(labels = c(x2 = "Santo Domingo", x3 = "Santiago")) +
  theme_stata()

# 2. Barras apiladas: ventas totales por año y fruta, coloreadas por ciudad
# Reusa 'dodge' en geom_col si se desea agrupado; aquí sin dodge para apilado
ggplot(long_bar, aes(x = factor(date), y = ventas, fill = ciudad)) +
  geom_col() +
  facet_wrap(~ x1) +
  labs(
    title = "Ventas Apiladas Anuales por Fruta",
    x     = "Año",
    y     = "Ventas",
    fill  = "Ciudad:"
  ) +
  scale_fill_stata(labels = c(x2 = "Santo Domingo", x3 = "Santiago")) +
  theme_stata()

# 3. Barras horizontales: ventas promedio por fruta (2019–2023)
avg_bar <- data_bar %>%
  group_by(x1) %>%
  summarise(
    SD       = mean(x2),
    Santiago = mean(x3)
  ) %>%
  pivot_longer(cols = c(SD, Santiago), 
               names_to = "ciudad", 
               values_to = "ventas")

ggplot(avg_bar, aes(y = ventas, x = fct_reorder(x1, ventas), fill = ciudad)) +
  geom_col(position = dodge, width = 0.6) +
  labs(
    title = "Ventas Promedio por Fruta y Ciudad (2019–2023)",
    x     = "Ventas Promedio",
    y     = "Fruta",
    fill  = "Ciudad:"
  ) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = scales::comma(ventas)), position = dodge, 
            hjust = 0.5, size = 3, vjust = -0.65) +
  scale_fill_stata(labels = c(SD = "Santo Domingo", 
                              Santiago = "Santiago")) +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))


# Panel -------------------------------------------------------------------

# Importar datos de panel
data_panel <- read_excel("data.xlsx", sheet = "panel")

# Asegurar que date sea numérico o Date
# Si date es numérico (ej. año), lo convertimos a Date al 1ro de enero
data_panel <- data_panel %>%
  mutate(
    date = as.Date(paste0(date, "-01-01"))
  )

# Transformar a formato largo para x1, x2, x3
long_panel <- data_panel %>%
  pivot_longer(
    cols      = c(x1, x2, x3),
    names_to  = "variable",
    values_to = "valor"
  )

# Plot: trayectorias de cada variable por id
ggplot(long_panel, aes(x = date, y = valor, color = variable)) +
  geom_line(size = 0.8) +
  scale_color_stata() +
  theme_stata() +
  labs(
    title = "Evolución de x1, x2 y x3 por entidad (panel)",
    x     = "Fecha",
    y     = "Valor",
    color = "Variable"
  ) +
  facet_wrap(~ id, scales = "free_y")

# Opcional: línea de tendencia global por variable (sin facet)
ggplot(long_panel, aes(x = date, y = valor, color = variable)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_stata() +
  theme_stata() +
  labs(
    title = "Tendencia lineal de cada variable sobre el tiempo",
    x     = "Fecha",
    y     = "Valor",
    color = "Variable"
  )
