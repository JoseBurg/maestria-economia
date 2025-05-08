library(quantmod)


quantmod::getSymbols(
  "USSTHPI",
  src = "FRED",
  frecu
)

index_price_usa <- USSTHPI


plot(index_price_usa)




