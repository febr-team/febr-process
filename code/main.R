# Calcular contribuições e gerar metadados para website

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")

# Descarregar chaves de identificação das planilhas do repositório
sheets_keys <- googlesheets::gs_read_csv(googlesheets::gs_key("18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q"))

# Preparar metadados para website (todos)
t0 <- proc.time()
# idx <- 1:nrow(sheets_keys)
idx <- nrow(sheets_keys)
# idx <- 5
lapply(idx, function (i) {
  x <- unlist(sheets_keys[i, ])
  createSiteMetadata(
    n = x["ctb"], dataset = x["dataset"], observation = x["observacao"], layer = x["camada"], 
    metadata = x["metadado"], sharing = x["compartilha"])
})
proc.time() - t0
