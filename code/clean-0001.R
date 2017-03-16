# Contribuição 0001
# Responsável: José Maria Filippini Alba
# Instituição: Embrapa Clima Temperado

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")

# Carregar dados
db <- read.csv("data/raw/fe0001/FeEETB.csv", head = TRUE, stringsAsFactors = FALSE)
db <- db[-nrow(db), ]
db

# Identificar linhas e colunas contendo dados de ferro
id_col <- colnames(db)[grep("Fe", colnames(db))]

# Salvar arquivo com descrição da contribuição para publicação no website 
ctb <- data.frame(Nome = "José Maria Filippini Alba",
                  Instituição = "Embrapa Clima Temperado",
                  UF = "RS",
                  Contribuição = sum(!is.na(db[id_col])) * 0.5,
                  Tipo = "EDAFOLÓGICO")
rownames(ctb) <- NULL
ctb
write.csv(ctb, "./web/data/ctb0001.csv")
rm(ctb)

