# Contribuição 0003
# Responsável: Sistema de Informação de Solos Brasileiros
# Instituição: Embrapa Informática Agropecuária / Embrapa Solos

# Com esse script são processados os dados para 

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")
source("code/helper-utf8.R", encoding = "UTF-8")

# Carregar dados
db <- read.csv(
  "data/raw/fe0003/embrapa-pos-03.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")


colnames(db)
cols <- c(
  # dataset
  "Título.do.Trabalho", "Autor", "Ano.de.Publicação", "Número", "Tipo.de.Publicação",
  "Nível.do.Levantamento.Pedológico", "Referência.Bibliográfica", 
  
  # observation
  "Código.PA", "Número.PA", "Número.de.Campo", "observation_date", "x_coord", "y_coord", "Município", "UF",
  "Classificação.Original", "Classificação.Atual", "Classificação.FAO.WRB", "Classificação.Soil.Taxonomy",
  "litology", "land_use", "Uso.Atual", "Descrição.Original", "Informações.Complementares",
  
  # layer
  "Símbolo.Horizonte", "Código.Horizonte",
  "Profundidade.Superior", "Profundidade.Inferior",
  "Ataque.sulfúrico...Fe2O3", "CDB...Ferro..g.kg.", "Oxalato.de.Amônio...Ferro",
  "Pirofosfato.de.Sódio...Ferro", "Microelementos...Ferro"
)



# Dividir dados por trabalho
db <- split(db, as.factor(db$Código.Trabalho))
colnames(db[[1]])
# lapply(1:length(db), function (i) {
  
#   write.table(
#     db[[i]][, cols], file = paste("data/raw/", i, ".csv", sep = ""), sep = "\t", 
#     fileEncoding = "UTF-8", row.names = FALSE)
# })
