pib_nominal <- databcrd::get_pib_gasto()

plot_variables <- function(
                    x = "Consumo", 
                    y = "Capital Fijo"){
  data_to_plot <- pib_nominal |>
    dplyr::filter(
      stringr::str_detect(
        partida, ifelse(!is.null(x), glue::glue("{x}|{y}"), glue::glue("{y}")))) |> 
    tidyr::pivot_wider(
      id_cols     = fecha, 
      names_from  = partida, 
      values_from = pib_nominal) |> 
    janitor::clean_names()
  
  if (!is.null(x)) {
  plot <- ggplot2::ggplot() + 
    ggplot2::geom_point(
      ggplot2::aes(
        x = data_to_plot[[2]], 
        y = data_to_plot[[3]])) +
    ggplot2::labs(
      x = x, 
      y = y)
  } else {
    plot <- ggplot2::ggplot() + 
      ggplot2::geom_point(
        ggplot2::aes(
          x = data_to_plot$fecha, 
          y = data_to_plot[[2]])) + 
      ggplot2::labs(
        x = "fecha", 
        y = y)
  }
  
  plot +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "lightblue")
}

plot_variables(
  x = "Producto",
  y = "Capital Fijo"
) 


producto <- plotl

plot_variables(
  x = NULL,
  y = "Producto"
)
plot_variables(
  x = "Importaciones",
  y = "Producto"
)

plot_variables(
  x = "Fecha",
  y = "Producto"
)
