library(dplyr)

tasas_pasivas <- databcrd::get_tasas_pasivas()
tasas_pasivas |> 
  filter(
    mes == "Diciembre",
    year >= 2008) |> 
  select(year, tp_interbancarios) |> 
  clipr::write_clip()


pib_nominal <- databcrd::get_pib_gasto() |> 
  filter(partida == "Producto Interno Bruto")

pib_nominal |> 
  filter(
    trimestre == 4,
    year >= 2008) |> 
  select(year, pib_nominal) |> 
  clipr::write_clip()
