estat_covid <- datos %>% 
  filter(year >= 2020) |> 
  select(-year, -starts_with("var")) %>% 
  summarise(across(
    everything(),
    list(
      Media      = ~mean(.x, na.rm = TRUE),
      Mediana    = ~median(.x, na.rm = TRUE),
      `Desv. Est` = ~sd(.x, na.rm = TRUE),
      Mínimo     = ~min(.x, na.rm = TRUE),
      Máximo     = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.fn}_{.col}"
  )) %>% 
  # Pone cada estadístico en su propia fila
  pivot_longer(
    everything(),
    names_to  = c("Estadístico", "Variable"),
    names_sep = "_",
    values_to = "Valor"
  ) %>% 
  pivot_wider(names_from = Variable, values_from = Valor)

estat_antes_covid <- datos %>% 
  filter(year < 2020) |> 
  select(-year, -starts_with("var")) %>% 
  summarise(across(
    everything(),
    list(
      Media      = ~mean(.x, na.rm = TRUE),
      Mediana    = ~median(.x, na.rm = TRUE),
      `Desv. Est` = ~sd(.x, na.rm = TRUE),
      Mínimo     = ~min(.x, na.rm = TRUE),
      Máximo     = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.fn}_{.col}"
  )) %>% 
  # Pone cada estadístico en su propia fila
  pivot_longer(
    everything(),
    names_to  = c("Estadístico", "Variable"),
    names_sep = "_",
    values_to = "Valor"
  ) %>% 
  pivot_wider(names_from = Variable, values_from = Valor)


estat_antes_covid_table <- estat_antes_covid %>% 
  gt(rowname_col = "Estadístico") %>%
  fmt_number(columns = everything(), decimals = 1, use_seps = TRUE) %>% 
  tab_header(
    title = "Estadísticos descriptivos",
    subtitle = "Antes-covid (2008-2019)"
  )

estat_covid_table <- estat_covid %>% 
  gt(rowname_col = "Estadístico") %>%
  fmt_number(columns = everything(), decimals = 1, use_seps = TRUE) %>% 
  tab_header(
    title = "Estadísticos descriptivos",
    subtitle = "Post-covid (2020-2024)"
  )

