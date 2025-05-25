library(readxl)
library(tidyverse)
library(lmtest)
library(sandwich)



# File in excel (size: 42.7 MB)
# engi <- read_excel("materias/econometria/proyecto-final/engi.xlsx")
# 
# # File in rsd (size: 4.4 MB)
# saveRDS(engi, "materias/econometria/proyecto-final/engi.rds")
theme_em <- function(text_size = 11, font_family = 'Gotham Book') { #el text size recomendado para informes es 13
  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", family = font_family, size = 16),
      axis.title = ggplot2::element_text(size = text_size, face = "bold"),
      axis.text = ggplot2::element_text(size = text_size),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 11),
      legend.position = "bottom",
      text = ggplot2::element_text(family = font_family)
    )
}
engi <- readRDS(here::here("materias/econometria/proyecto-final/engi.rds")) |> 
  janitor::clean_names()

engi_transformado <- engi |> 
  filter(stringr::str_detect(grupo_empleo, "Formal|Informal"),
         # salario_principal > 0 & 
         # salario_principal < quantile(salario_principal, 0.98) # Quitando cola superior
         ) |> 
  mutate(
    edad = case_when(
      orden_edad == 1 ~ 19.5,
      orden_edad == 2 ~ 32,
      orden_edad == 3 ~ 49.5,
      orden_edad == 4 ~ 79),
    sector = ifelse(grupo_empleo == "Empleo Formal", 1, 0),
    # ahorra?:
    ahorra = ifelse(d704b == 1, 1, 0), 
    ) |> 
  rowwise() |> 
  mutate(
    ingreso = salario_principal + salario_secundario,
    ahorro_monto = interes_ahorro + retiro_ahorros + cobro_san + interes_certificados +
      recuperacion_dinero_prestado + interes_prestamo
  )

summary(engi_transformado$ahorro_monto[engi_transformado$ahorro_monto>0])

# Variables de intereses:
# salario_principal
# antiguedad_princ
# escolaridad
# grupo_sector
# ahorro = interes_ahorro + retiro_ahorros

modelo1 <- lm(
  ingreso ~ escolaridad + edad + sector,
  data = engi_transformado
)

stargazer::stargazer(modelo1, type = "text")




engi_transformado |> 
  # filter(salario_principal <= quantile(salario_principal, 0.85)) |> 
  ggplot(aes(salario_principal)) + 
  geom_histogram()

engi_transformado |> 
  mutate(sector = ifelse(sector == 1, "Formal", "Informal")) |> 
  ggplot(aes(ingreso)) + 
  geom_histogram() + 
  facet_wrap(vars(sector)) + 
  theme_em() + 
  labs(x = "Salario", y = NULL)
  




# Test del modelo ---------------------------------------------------------

# Errores robustos HC3
coeftest(modelo1, vcov = vcovHC(modelo1, type = "HC3"))

# Prueba de Breusch-Pagan
bptest(modelo1)

# Incluir no linealidad en edad
modelo2 <- update(modelo1, . ~ . + I(edad^2))



# Ahorro ------------------------------------------------------------------


plot(engi_transformado$ahorro_monto[engi_transformado$ahorro_monto>0])



modelo_ahorro <- lm(ahorro_monto ~ ingreso + escolaridad,
                    data = engi_transformado |> 
                      filter(ahorro_monto>0))


stargazer::stargazer(modelo_ahorro, type = "text")


