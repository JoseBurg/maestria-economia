
get_cotizante <- function() {
  
  `%>%` <- magrittr::`%>%`
  
  cotizantes_sp <- "https://www.one.gob.do/media/wskbjaux/cotizantes-del-sistema-de-pensiones-por-sexo-seg%C3%BAn-mes-2004-2024.xlsx"
  
  # Descargando el archivo temporal
  temporal_file <- tempfile(fileext = '.xlsx')
  
  # Descargando el archivo
  response <- httr::GET(cotizantes_sp)
  bin <- httr::content(response, 'raw')
  writeBin(bin, temporal_file)
  
  # Reading the file
  readxl::read_excel(temporal_file, skip = 4) %>%
    setNames(c('mes', 'total', 'hombres', 'mujeres')) %>%
    dplyr::filter(!is.na(mes)) %>%
    dplyr::mutate(year = ifelse(stringr::str_detect(mes, '^[0-9]+$'), mes, NA)) %>%
    tidyr::fill(year) %>%
    dplyr::filter(!is.na(total)) %>%
    dplyr::mutate(
      year = as.numeric(year),
      mes = databcrd::crear_mes(mes),
      periodo = lubridate::make_date(year = year, month = mes, day = '01')
    ) %>%
    dplyr::select(-c(mes, year)) %>% 
    dplyr::relocate(periodo) |> 
    tsibble::as_tibble() |> 
    suppressMessages()
}
