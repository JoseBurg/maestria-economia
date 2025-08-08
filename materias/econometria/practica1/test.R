data_to_excel <- function(
  data,
  name_sheet = "sheet1",
  zoom = 100
) {
  tempfile <- tempfile(".xlsx")

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(
    wb,
    sheetName = name_sheet,
    zoom = zoom,
    fontName = "Arial"
  )

  # Write data
  data <- data |>
    janitor::clean_names("title")

  openxlsx::writeData(wb, name_sheet, data)

  # freeze col and rows first
  openxlsx::freezePane(wb, name_sheet, firstRow = TRUE, firstCol = TRUE)

  style_level1 <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#8d99ae",
    fontSize = 14,
    halign = "center",
    valign = "center"
  )

  openxlsx::addStyle(
    wb,
    name_sheet,
    style = style_level1,
    rows = 1,
    cols = 1:11
  )

  openxlsx::openXL(wb)
}

convert_excel(data = mtcars)
