# Calcular contribuições e gerar metadados para website

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")
library(googlesheets)

# Descarregar chaves de identificação das planilhas do repositório
sheets_keys <- gs_read_csv(gs_key("18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q"))

# Preparar metadados para website (todos)
t0 <- proc.time()
lapply(1:nrow(sheets_keys), function (i) {
  x <- unlist(sheets_keys[i, ])
  createSiteMetadata(
    n = x["ctb"], dataset = x["dataset"], observation = x["observacao"], layer = x["camada"], 
    metadata = x["metadado"], sharing = x["compartilha"])
})
proc.time() - t0

# Preparar metadados para website (última)
# x <- sheets_keys[nrow(sheets_keys), ]
# createSiteMetadata(x["ctb"], x["dataset"], x["observacao"], x["camada"], x["metadado"], x["compartilha"])
