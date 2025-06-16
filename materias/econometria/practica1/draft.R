library(readr)
library(dplyr)

encft <- read_csv("materias/econometria/practica1/datos/encft2016.csv") |> 
  janitor::clean_names()

encft <- encft |> 
  mutate(
    #Cuestionario: mujer = 2 y hombre = 1 
    mujer = ifelse(sexo == 2, 1, 0)
  )

library(dplyr)
library(ggplot2)

encft |> 
  mutate(
    mujer = factor(mujer, levels = c(0, 1),
                   labels = c("Hombre", "Mujer"))) |> 
  ggplot(aes(x = edad, fill = mujer)) + 
  geom_histogram(bins = 30, alpha = 0.8, position = "identity") +
  scale_fill_manual(
    name   = "Género",
    values = c("Hombre" = "#1f78b4", "Mujer" = "#e31a1c"),
    labels = c("Hombre", "Mujer")) +
  facet_wrap(~ mujer) +
  labs(
    title    = "Distribución de edad por género",
    x        = "Edad",
    y        = "Frecuencia") +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
    
  )
encft$forma_pagan_salario_ap
plot(encft$ingreso_asalariado
  
)


koopand <- readxl::read_xls("./materias/econometria/practica1/datos/KoopandTobias2004.xls") |> 
  janitor::clean_names()


modelo1 <- lm(wage ~ education + experience + ability, data = koopand)


coef(modelo1)
