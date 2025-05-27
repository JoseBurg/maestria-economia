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