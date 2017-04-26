# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")

n <- "ctb0008"

# Descarregar dados
dataset <- as.data.frame(gsheet::gsheet2tbl(
  "https://docs.google.com/spreadsheets/d/1Vk4NZkzjQ_xjponhrSBw8y1ue6-yXVpbOSzYTUt4S8Q/edit#gid=1085102806"
))
observations <- gsheet::gsheet2tbl(
  "https://docs.google.com/spreadsheets/d/1Vk4NZkzjQ_xjponhrSBw8y1ue6-yXVpbOSzYTUt4S8Q/edit#gid=133250549"
)
layer <- gsheet::gsheet2tbl(
  "https://docs.google.com/spreadsheets/d/1Vk4NZkzjQ_xjponhrSBw8y1ue6-yXVpbOSzYTUt4S8Q/edit#gid=72748886"
)

# Agregar dados das observações e camadas
db <- merge(observations, layer, by = "observation_id")

# Identificar linhas e colunas contendo dados de ferro
id_col <- colnames(db)[grep("fe", colnames(db))]
idx <- which(!is.na(db[, id_col]), arr.ind = TRUE)
if (is.null(dim(idx))) {
  id_row <- idx
} else {
  id_col <- id_col[unique(idx[, 2])]
  id_row <- unique(idx[, 1])
}

# Preparar descrição da contribuição 
ctb <- data.frame(
  Nome = stringr::str_split_fixed(dataset[dataset$item == "author_name", "data"], ";", n = Inf)[1],
  Instituição = dataset[dataset$item == "organization_name", 2],
  UF = levels(as.factor(db[id_row, "state_id"])),
  Contribuição = summary(as.factor(db[id_row, "state_id"])),
  Tipo = ifelse(
    dataset[dataset$item == "subject", "data"] == "Gênese, Morfologia e Classificação dos Solos",
    "PEDOLÓGICO", "EDAFOLÓGICO"),
  url = "https://docs.google.com/spreadsheets/d/1Vk4NZkzjQ_xjponhrSBw8y1ue6-yXVpbOSzYTUt4S8Q/edit?usp=sharing")
rownames(ctb) <- NULL
ctb

# Salvar arquivo com descrição da contribuição para publicação no website
write.csv(ctb, paste("./web/data/", n, ".csv", sep = ""), fileEncoding = "UTF-8")
rm(ctb)
