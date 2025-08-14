library(VGAM)
library(censReg)
library(GGally)
library(ggplot2)
library(dplyr)

pension <- readxl::read_excel(
  "materias/Seminario de economía/trabajo final/pension.xlsx"
) |>
  # Vamos a filtrar que las variables no sean negativas
  filter(wealth89 >= 0, pctstck >= 0)


# La variable pyears es el número de años que la persona ha estado en el trabajo

# Sección de gráficos -----------------------------------------------------

# Distribución de la variable dependiente
ggplot(pension, aes(x = pctstck)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  labs(title = "Distribución del porcentaje invertido en acciones")
# Riqueza según distribución del porcentaje invertido en acciones
library(viridis)
pension |>
  mutate(
    pctstck = factor(
      pctstck,
      levels = c(0, 50, 100),
      labels = c("0%", "50%", "100%")
    )
  ) |>
  ggplot(aes(x = pctstck, y = wealth89, fill = pctstck)) +
  # Ecluir los valores atípicos
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, option = "B") +
  geom_jitter(
    color = "#00a6fb",
    alpha = 0.3,
    shape = 16,
    position = position_jitter(width = 0.2, height = 0)
  ) +
  labs(
    x = "Porcentaje invertido en acciones",
    y = "Riqueza neta en 1989"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # Colocar el text font de times new roman
    text = element_text(family = "Times New Roman")
  )


# Educación según según el porcentaje invertido en acciones
ggplot(pension, aes(x = educ, y = age)) +
  geom_point()

# Correlaciones entre variables numéricas
# Para este gráfico vamos a cambiar los nombres, que seán nombres descriptivos
# y cortos:
pension |>
  rename(
    riqueza89 = wealth89,
    porcentaje_acciones = pctstck,
    educacion = educ,
    edad = age
  ) |>
  select(porcentaje_acciones, educacion, riqueza89, edad) |>
  ggpairs() + # quitar las lineas de fondo del gráfico con theme
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_blank()
    # panel.grid.minor = element_blank()
  )

pension |>
  select(pctstck, educ, wealth89, age) |>
  ggpairs() +
  theme_bw()

# Gráfico de densidad age
ggplot(pension, aes(x = age)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "Densidad de la edad")

# Gráfico de densidad wealth89
ggplot(pension, aes(x = wealth89)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "Densidad de la riqueza neta en 1989")

# Distribución de la variable riqueza neta en 1989
ggplot(pension, aes(x = wealth89)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  labs(title = "Distribución de la riqueza neta en 1989")

# Porcentaje de acciones según matrimonio
#

# Modelo lineal básico
modelo_lineal <- lm(
  pctstck ~ educ + wealth89 + age + married + irain89,
  data = pension
)

stargazer::stargazer(
  modelo_lineal,
  type = "text",
  title = "Modelo Lineal Básico"
)
# Modelo tobit: ---------------------------------------------------------
library(AER)

m_tobit_aer <- tobit(
  pctstck ~ educ + wealth89 + age + married + irain89,
  left = 0,
  right = 100,
  data = pension
)

stargazer::stargazer(m_tobit_aer, type = "text", title = "Modelos Tobit")

library(nnet) # -------------------------------------------------

pension <- pension %>%
  mutate(pctstck_fac = factor(pctstck))

modelo_mult <- multinom(
  pctstck_fac ~ educ + wealth89 + age + married + irain89,
  data = pension
)

stargazer::stargazer(modelo_mult, type = "text", title = "Modelo Multinomial")

# Análisis de la bondad de ajuste del modelo multinomial
library(DescTools)
PseudoR2(modelo_mult, which = "McFadden")

library(car)
vif(glm(
  I(pctstck > 0) ~ educ + wealth89 + age + married + irain89,
  data = pension,
  family = binomial
))


library(mlogit)
# Convertir datos a formato mlogit
pension_mlogit <- mlogit.data(pension, choice = "pctstck_fac", shape = "wide")
mlogit_model <- mlogit(
  pctstck_fac ~ 0 | educ + wealth89 + age + married + irain89,
  data = pension_mlogit,
  reflevel = "0"
)

summary(mlogit_model)

pred <- predict(modelo_mult, type = "class")
table(Predicho = pred, Real = pension$pctstck_fac)
mean(pred == pension$pctstck_fac)

# Estadisticas descriptivas sobre las variables:
pension |>
  summarise(
    across(
      c(pctstck, educ, wealth89, age),
      list(
        media = mean,
        mediana = median,
        sd = sd,
        min = min,
        max = max
      ),
      na.rm = TRUE
    )
  ) |> # Ajustar una tabla de resumen con su nombre correcto, pasar de wide a long
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_sep = "_"
  ) # colocar nombre correctamente
