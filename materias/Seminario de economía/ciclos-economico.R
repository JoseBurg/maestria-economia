# Asegurarse de que las librerías necesarias estén cargadas
library(ggplot2)
library(tidyr)
library(dplyr)

imae <- databcrd::get_imae() |> 
  select(
    fecha, indice_original
  )


# Asumiendo que 'imae' ya está cargado y tiene las columnas 'fecha' e 'indice_original'

# Convertir el tibble a un objeto de serie temporal (ts)
# Necesitamos la fecha de inicio y la frecuencia (mensual = 12)
start_date <- min(imae$fecha)
start_year <- as.numeric(format(start_date, "%Y"))
start_month <- as.numeric(format(start_date, "%m"))

imae_ts <- ts(imae$indice_original, start = c(start_year, start_month), frequency = 12)

# Realizar la descomposición STL
# s.window = "periodic" es común para datos mensuales con estacionalidad constante
# t.window controla la suavidad de la tendencia
decomposition <- stl(imae_ts, s.window = "periodic", t.window = 12) # Puedes ajustar t.window

# Extraer los componentes y combinarlos con las fechas originales
# Convertir el objeto stl a un data frame
decomp_df <- as.data.frame(decomposition$time.series)

# Añadir la columna de fecha del tibble original
decomp_df$fecha <- imae$fecha

# Añadir la serie original al data frame
decomp_df$original <- imae$indice_original

# Renombrar la columna 'remainder' a 'ciclo' para mayor claridad
decomp_df <- decomp_df %>%
  rename(ciclo = remainder)

# Transformar el data frame a formato largo para ggplot2
decomp_long <- decomp_df %>%
  pivot_longer(
    cols = c(original, trend, seasonal, ciclo), # Columnas a pivotar
    names_to = "componente", # Nombre de la nueva columna para los nombres de las columnas originales
    values_to = "valor" # Nombre de la nueva columna para los valores
  )

# Reordenar los niveles del factor 'componente' para el orden de los gráficos
decomp_long$componente <- factor(decomp_long$componente,
                                 levels = c(
                                   "ciclo",
                                   "original", "trend", "seasonal"
                                 ))

# Crear el gráfico con ggplot2
ggplot(decomp_long, aes(x = fecha, y = valor)) +
  geom_line() +
  facet_wrap(~ componente, scales = "free_y", ncol = 1) + # Un gráfico por componente, ejes Y libres
  labs(title = "Descomposición STL del IMAE",
       x = "Fecha",
       y = "Valor") +
  theme_minimal() # O cualquier otro tema de ggplot2 que prefieras


# Filtros para ciclos economicos ------------------------------------------

# Asegurarse de que las librerías necesarias estén cargadas
library(mFilter)
library(ggplot2)
library(tidyr)
library(dplyr) # Ya la tienes cargada

# Asumiendo que 'imae' ya está cargado y tiene las columnas 'fecha' e 'indice_original'
# Y que 'imae_ts' ya fue creado a partir de 'imae'

# --- Aplicar Filtros ---

# 1. Filtro Hodrick-Prescott (HP)
# lambda es el parámetro de suavizado. Para datos mensuales, 14400 es un valor común.
hp_filter <- hpfilter(imae_ts, freq = 14400)
hp_cycle <- hp_filter$cycle

# 2. Filtro Baxter-King (BK)
# pl: periodicidad mínima del ciclo (en meses). Ej: 18 meses.
# pu: periodicidad máxima del ciclo (en meses). Ej: 96 meses (8 años).
# k: número de rezagos/adelantos a usar. Un valor común es 12 para datos mensuales.
# Nota: El filtro BK pierde observaciones al principio y al final (k observaciones).
bk_filter <- bkfilter(imae_ts, pl = 18, pu = 96, k = 12)
bk_cycle <- bk_filter$cycle

# 3. Filtro Christiano-Fitzgerald (CF)
# similar a BK, pero maneja mejor los puntos finales.
# low: periodicidad mínima (en meses)
# high: periodicidad máxima (en meses)
cf_filter <- cffilter(imae_ts, low = 18, high = 96)
cf_cycle <- cf_filter$cycle

# --- Preparar datos para ggplot2 ---

# Crear un data frame con las fechas y los ciclos de cada filtro
# Nota: Los filtros BK y CF pueden tener menos puntos que la serie original debido a los rezagos/adelantos
# Aseguramos que las fechas coincidan con los ciclos extraídos
dates_for_filtered <- imae$fecha[1:length(hp_cycle)] # Asumimos que HP tiene la misma longitud

# Crear el data frame
cycles_df <- data.frame(
  fecha = dates_for_filtered,
  hp = as.numeric(hp_cycle),
  # Para BK y CF, necesitamos alinear las fechas correctamente
  # bk_cycle y cf_cycle tienen length(imae_ts) - 2*k observaciones
  bk = c(rep(NA, bk_filter$k), as.numeric(bk_cycle), rep(NA, bk_filter$k)),
  cf = as.numeric(cf_cycle)
)

# Transformar a formato largo para ggplot2
cycles_long <- cycles_df %>%
  pivot_longer(
    cols = c(hp, bk, cf),
    names_to = "filtro",
    values_to = "ciclo"
  ) %>%
  # Eliminar NAs introducidos por el filtro BK para la visualización
  filter(!is.na(ciclo))

# --- Graficar con ggplot2 ---

ggplot(cycles_long, aes(x = fecha, y = ciclo, color = filtro)) +
  geom_line() +
  labs(title = "Componentes Cíclicos del IMAE (Comparación de Filtros)",
       x = "Fecha",
       y = "Ciclo",
       color = "Filtro") +
  theme_minimal()

