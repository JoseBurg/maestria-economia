library(pdftools)

file <- "C:/Users/JoseA/OneDrive/Escritorio/OneDrive/Maestría en economía/2. Econometría/book - wooldridge-2009-introduccic3b3n-a-la-econometrc3ada-un-enfoque-moderno.pdf"
pdf_text <- pdf_text(file)


# Convertir a un json para agilizar la lectura

library(jsonlite)
json_file <- "C:/Users/JoseA/OneDrive/Escritorio/OneDrive/Maestría en economía/2. Econometría/book - wooldridge-2009-introduccic3b3n-a-la-econometrc3ada-un-enfoque-moderno.json"
write_json(pdf_text, json_file)
# Leer el json
json_data <- fromJSON(json_file)

# Convertir a un data frame
pdf_df <- data.frame(page = seq_along(json_data), text = json_data, stringsAsFactors = FALSE)


