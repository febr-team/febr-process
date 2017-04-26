# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")

n <- "ctb0672"

# Descarregar dados
dataset <- as.data.frame(gsheet::gsheet2tbl(
  "https://docs.google.com/spreadsheets/d/1_lp9UHUnKi0Y6kSg_OEno-xh_BzGvqin4qBdFmpdZ8M/edit#gid=1691823338"
))
observations <- gsheet::gsheet2tbl(
  "https://docs.google.com/spreadsheets/d/1HyAVwK1comUPvErDOOEq80KfDazNi1LNd1uimjLMEv4/edit#gid=112014124"
)
layer <- gsheet::gsheet2tbl(
  "https://docs.google.com/spreadsheets/d/1ohV68-30TUF-zmUL0t_aPsar84E71UElRT5aEb2i6F8/edit#gid=193673171"
)

# Agregar dados das observações e camadas
db <- merge(observations, layer, by = "observation_id")

# Identificar linhas e colunas contendo dados de ferro
id_col <- colnames(db)[grep("^fe_", colnames(db))]
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
  Instituição = stringr::str_split_fixed(dataset[dataset$item == "organization_name", 2], ";", n = Inf)[1],
  UF = levels(as.factor(db[id_row, "state_id"])),
  Contribuição = summary(as.factor(db[id_row, "state_id"])),
  Tipo = ifelse(
    dataset[dataset$item == "subject", "data"] == "Gênese, Morfologia e Classificação dos Solos",
    "PEDOLÓGICO", "EDAFOLÓGICO"),
  url = "https://drive.google.com/drive/folders/0B7xsLbrOA23oRnRvRmlrMlRyQ1E?usp=sharing")
rownames(ctb) <- NULL
ctb

# Salvar arquivo com descrição da contribuição para publicação no website
write.csv(ctb, paste("./web/data/", n, ".csv", sep = ""), fileEncoding = "UTF-8")
rm(ctb)
