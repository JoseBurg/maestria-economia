# 1. Carga el paquete
library(gptstudio)

# 2. Define tu API key (si no la tienes en .Renviron)
Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))

# 3. Llama correctamente a query_openai_api
respuesta <- query_openai_api(
  task         = "chat.completions",
  request_body = list(
    model    = "gpt-3.5-turbo",
    messages = list(
      list(role = "system",  content = "Eres un asistente muy útil."),
      list(role = "user",    content = "Escribe un poema breve sobre el café.")
    )
  )
)  # :contentReference[oaicite:0]{index=0}

# 4. Extrae el texto
cat(respuesta$choices[[1]]$message$content)

result <- chat(
  prompt = "Write a simple function in R",
  skill = "advanced",
  style = "tidyverse",
  task = "coding"
)

