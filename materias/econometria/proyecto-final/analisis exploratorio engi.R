library(readxl)
library(tidyverse)

# File in excel (size: 42.7 MB)
# engi <- read_excel("materias/econometria/proyecto-final/engi.xlsx")
# 
# # File in rsd (size: 4.4 MB)
# saveRDS(engi, "materias/econometria/proyecto-final/engi.rds")

engi <- readRDS("materias/econometria/proyecto-final/engi.rds")

engi_transformado <- engi |> 
  janitor::clean_names() |> 
  filter(stringr::str_detect(grupo_empleo, "Formal|Informal")) |> 
  mutate(
    # ahorra?:
    sector = ifelse(grupo_empleo == "Empleo Formal", 1, 0),
    ahorra = ifelse(d704b == 1, 1, 0), 
    ) |> 
  rowwise() |> 
  mutate(ahorro_monto = (interes_ahorro + retiro_ahorros))


# Variables de intereses:
# salario_principal
# antiguedad_princ
# escolaridad
# grupo_sector
# ahorro = interes_ahorro + retiro_ahorros

modelo1 <- lm(
  salario_principal ~ escolaridad + antiguedad_princ + sector,
  data = engi_transformado
)

stargazer::stargazer(modelo1, type = "text")


engi_transformado |> 
  # filter(salario_principal <= quantile(salario_principal, 0.85)) |> 
  ggplot(aes(antiguedad_princ)) + 
  geom_histogram()
  