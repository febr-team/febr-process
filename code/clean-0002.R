# Contribuição 0002
# Responsável: Elias Mendes Costa
# Instituição: Universidade Federal Rural do Rio de Janeiro

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")

# Carregar dados
db <- read.csv("data/raw/fe0002/Costa2015.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE)

# Identificar linhas e colunas contendo dados de ferro
id_col <- colnames(db)[grep("Fe", colnames(db))]
id_col <- id_col[-2]
idx <- which(!is.na(db[, id_col]), arr.ind = TRUE)
id_col <- id_col[unique(idx[, 2])]
id_row <- unique(idx[, 1])

# Salvar arquivo com descrição da contribuição para publicação no website 
ctb <- data.frame(Nome = "Elias Mendes Costa",
                  Instituição = "Universidade Federal Rural do Rio de Janeiro",
                  UF = "RJ",
                  Contribuição = sum(!is.na(db[id_row, id_col])),
                  Tipo = "PEDOLÓGICO")
rownames(ctb) <- NULL
ctb
write.csv(ctb, "./web/data/ctb0002.csv")
rm(ctb)

# PERFIS ######################################################################################################
pf <- db[id_row, c(
  "Código.PA", "Data.da.Coleta", "Datum", "Northing", "Easting", "UF", "Município", "Classe.de.Solos.Nível.3",
  "X1ª.Ocorrência", "Uso.Atual", "Litologia")]
pf <- pf[!duplicated(pf$Código.PA), ]

# PERFIS: Data de observação ----
pf$Data.da.Coleta <- stringr::str_replace(pf$Data.da.Coleta, "/", "-")
pf$Data.da.Coleta <- stringr::str_replace(pf$Data.da.Coleta, "/", "-")

# PERFIS: Classificação do solo ----
pf$classe <- paste(pf$Classe.de.Solos.Nível.3, " ", pf$X1ª.Ocorrência, sep = "")
Encoding(pf$classe) <- "UTF-8"
pf$sibcs <- sapply(pf$classe, sibcsSymbol)

# PERFIS: Uso da terra ----
# Apenas dois usos da terra foram identificados.
id <- grep("pastagem", pf$Uso.Atual, ignore.case = TRUE)
pf$Uso.Atual[id] <- "CRIAÇÃO ANIMAL"
pf$Uso.Atual[6] <- "CRIAÇÃO ANIMAL"
pf$Uso.Atual[10] <- "PROTEÇÃO DA NATUREZA"

# PERFIS: Litologia ----
id <- grep("aluvionar", pf$Litologia)
pf$Litologia[id] <- "Sedimento aluvial"

# Salvar arquivo temporário com dados para planilha final
write.csv(pf, "data/raw/fe0002/tmp.csv")
rm(pf)

# Remover arquivo temporário
system("rm data/raw/fe0002/tmp.csv")

# HORIZONTE ###################################################################################################
hz <- db[id_row, c(
  "Código.PA", "Código.Horizonte", "Símbolo.Horizonte", "Profundidade.Superior", "Profundidade.Inferior", 
  id_col)]
str(hz)

# Salvar arquivo temporário com dados para planilha final
write.csv(hz, "data/raw/fe0002/tmp.csv")

# Remover arquivo temporário
system("rm data/raw/fe0002/tmp.csv")
