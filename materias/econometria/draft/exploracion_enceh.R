library(dplyr)

enceh <- readxl::read_excel("materias/econometria/proyecto-final/datoscrudos-enceh.xlsx") |> 
  janitor::clean_names() |> 
  filter(!is.na(result_id))




data <- enceh |> 
  mutate(
    across(c(sit_ecn_pais, sit_pais_futuro, sit_econ_hogar, sit_econ_hogar_2), 
           ~recode(.x,
                  "Mucho peores"         = -1,
                  "Peores"               = -0.5,
                  "Iguales"              = 0,
                  "Mejores"              = 0.5,
                  "Mucho mejores"        = 1,
                  "No sabe (espontáneo)" = 0)),
    across(
      c(desempleo_hogar, ahorro_hogar, 
        ahorro_futuro, subcidios, tiene_empleo,
        remesas, improtancia_subs, solicito_prestam,
        tiene_prestamo, bancarizado),
      ~recode(.x,
              "Sí" = 1,
              "No" = 0)),
      across(c(
        numero_vivienda, 
        inflacion, cantidad_hogares, cantidad_miembro),
        as.numeric)
  ) |> 
  mutate(
    ingreso_num = case_when(
      str_starts(ingreso_mes, "Prefiero") ~ NA_real_,
      str_starts(ingreso_mes, "Menos de") ~ parse_number(ingreso_mes) / 2,
      str_starts(ingreso_mes, "Más de")   ~ parse_number(ingreso_mes) + 5000
    )
  ) %>% 
  # 2. procesar los rangos que quedan
  tidyr::separate(
    ingreso_mes, into = c("min", "max"), sep = " - ", fill = "right", remove = FALSE
  ) %>% 
  mutate(
    min = parse_number(min),
    max = parse_number(max),
    ingreso_num = coalesce(ingreso_num, (min + max) / 2)
  ) %>% 
  select(-min, -max) 




data_ice <- data |> 
  rowwise() |> 
  mutate(
    ICE = mean(
      c_across(c(sit_ecn_pais, sit_pais_futuro, sit_econ_hogar, sit_econ_hogar_2)),
      na.rm = TRUE
    )
  )

modelo <- lm(sit_ecn_pais ~ subcidios + ahorro_hogar, data = data_ice)

stargazer::stargazer(modelo, type = "text")


summary(modelo)
# 3. Verificación de la estructura final
glimpse(encuesta_transformada)
summary(encuesta_transformada)

# 4. Exportar datos transformados
write_csv(encuesta_transformada, "encuesta_transformada.csv")

