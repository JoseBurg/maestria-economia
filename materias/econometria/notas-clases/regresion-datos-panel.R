library(air)
library(wooldridge)


# air format
modelo <- lm(
  log(wage) ~ educ + exper + I(exper^2) + tenure,
  data = wooldridge::wage1
)


stargazer::stargazer(modelo, type = "text")
