# Calcular contribuições e gerar metadados para website

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")
library(magrittr)

# Descarregar chaves de identificação das planilhas do repositório
sheets_keys <- 
  googlesheets::gs_key("18yP9Hpp8oMdbGsf6cVu4vkDv-Dj-j5gjEFgEXN-5H-Q") %>%
  googlesheets::gs_read_csv()

# Preparar metadados para website (todos)
lapply(1:nrow(sheets_keys), function (i) {
  x <- sheets_keys[i, ]
  createSiteMetadata(x["ctb"], x["dataset"], x["observacao"], x["camada"], x["metadado"], x["compartilha"])
})

# Preparar metadados para website (última)
# x <- sheets_keys[nrow(sheets_keys), ]
# createSiteMetadata(x["ctb"], x["dataset"], x["observacao"], x["camada"], x["metadado"], x["compartilha"])
