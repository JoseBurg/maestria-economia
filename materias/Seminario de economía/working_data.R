library(xts)
library(tidyverse)

source("./materias/Seminario de economía/functions.R")


# Préstamos de la banca comercial en EAU --------------------------
credito_eua <- quantmod::getSymbols(
  Symbols = "TOTBKCR", 
  src = "FRED",
  from = "2022-01-01",
  auto.assign = FALSE
)

credito_eua_monthly <- xts::apply.monthly(credito_eua,last)

credito_eua <- data.frame(
  periodo    = as.Date(index(credito_eua_monthly)),
  p_priv_eua = coredata(credito_eua_monthly)) %>% 
  mutate(
    periodo = lubridate::make_date(year(periodo), month(periodo), "01")
  )


# Contizantes del sistema del sistema de pensiones

get_cotizante()



prestamos_sector_privado <- databcrd::get_prestamos_osd()

prestamos_sector_privado |> 
  filter(!sectores %in% c("PRÉSTAMOS DE CONSUMO", "OTROS PRÉSTAMOS DE CONSUMO")) |> 
  summarise(
    across(c(mn, me, consolidado), ~sum(.x, na.rm = TRUE)),
    .by = fecha
  )
