

sumatoria <- function(parametro) {
  
  .fun = list(
    Promedio = mean, 
    Mediana = median
  )
  

  
  parametro |> 
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        .fun, .names = "{.col}_{.fn}"
      )) |> 
    # browser()
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Variables",
      values_to = "Valor"
    ) |>  
    tidyr::separate_longer_delim(
      col = Variables,
      delim = "_"
    ) |> 
    gt::gt()
}


mtcars |> 
  sumatoria()

