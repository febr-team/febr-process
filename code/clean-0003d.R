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

# Definir colunas que serão exportadas
cols_dataset <- c(
  "Título.do.Trabalho",
  "Autor",
  "Ano.de.Publicação",
  "Número",
  "Tipo.de.Publicação",
  "Nível.do.Levantamento.Pedológico",
  "Referência.Bibliográfica",
  "Código.Trabalho")
cols_observation <- c(
  "Código.PA",
  "Número.PA",
  "Número.de.Campo",
  "observation_date",
  "x_coord", 
  "y_coord",
  "Município", "UF",
  "Classificação.Original",
  "Classificação.Atual",
  "Classificação.FAO.WRB",
  "Classificação.Soil.Taxonomy",
  "Litologia",
  "land_use",
  "Uso.Atual",
  "Localização.descritiva", 
  "Responsável.is..pela.Descrição",
  "Tipo", 
  "Situação.coleta.das.amostras",
  "Material.de.Origem",
  "Informações.Complementares",
  "Observações",
  "Altitude..m.",
  "Fase.de.Pedregosidade",
  "Fase.de.Vegetação.Primária",
  "Fase.de.Relevo", 
  "Fase.de.Substrato", 
  "Forma.de.Relevo", 
  "Relevo.Local", 
  "Relevo.Regional", 
  "Grupo.Geológico", 
  "Formação.Geológica", 
  "Cronologia", 
  "Gaussen",
  "Köppen",
  "Saturação.por.bases.ou.por.alumínio",
  "Caráter.Aluminico.Alitico",
  "Caráter.Sodico.Solodico",
  "Caráter.Sálico.Salino",
  "Caráter.Carbonático.Carbonato",
  "Caráter.Plíntico.Plânico",
  "Caráter.Concrecionário.Litoplíntico.Dúrico",
  "Caráter.Ácrico",
  "Caráter.Flúvico",
  "Caráter.Argilúvico",
  "Caráter.Coeso",
  "Caráter.Vértico",
  "Caráter.Epiáquico",
  "Caráter.Crômico",
  "Caráter.Ebânico",
  "Caráter.Rúvico",
  "Caráter.Êutrico",
  "Atividade.da.argila",
  "Horizonte.diagnóstico.superficial",
  "Horizonte.diagnóstico.subsuperficial",
  "Grupamento.de.classe.textural.superficial",
  "Grupamento.de.classe.textural.subsuperficial",
  "Classe.de.pedregosidade",
  "Classe.de.rochosidade",
  "Classe.de.drenagem",
  "Classe.de.erosão",
  "Frequência.de.Erosão...1ª.Ocorrência",
  "Frequência.de.Erosão...2ª.Ocorrência",
  "Forma.de.Erosão...1ª.Ocorrência",
  "Forma.de.Erosão...2ª.Ocorrência",
  "Profundidade.de.Erosão...1ª.Ocorrência",
  "Profundidade.de.Erosão...2ª.Ocorrência",
  "Ocorrência.Autogranulação",
  "Presença.de.Concreções",
  "Presença.de.Contato.Lítico",
  "Presença.de.Fase.Erodida",
  "Presença.de.Fase.Rochosa",
  "Presença.Gilgai",
  "Presença.Contato.lico.fragmentário",
  "Presença.de.Nódulos",
  "Presença.Petroplintita",
  "Ocorrência.Plintita")
cols_layer <- c(
  "Código.PA",
  "Símbolo.Horizonte",
  "Código.Horizonte",
  "Profundidade.Superior",
  "Profundidade.Inferior",
  "Ataque.sulfúrico...Fe2O3",
  "CDB...Ferro..g.kg.",
  "Oxalato.de.Amônio...Ferro",
  "Pirofosfato.de.Sódio...Ferro",
  "Microelementos...Ferro")

# Dividir dados por trabalho
db <- split(db, as.factor(db$Código.Trabalho))

# Salvar trabalhos em arquivos individuais
file_name <- names(db)
lapply(1:length(db), function (i) {
  
  dataset <- t(unique(db[[i]][, cols_dataset]))
  write.table(
    dataset,
    file = paste("data/raw/", file_name[i], "-dataset.csv", sep = ""), 
    sep = "\t", fileEncoding = "UTF-8")
  
  observation <- unique(db[[i]][, cols_observation])
  observation$coord_accuracy <- NA
  observation$coord_source <- NA
  observation$country_id <- "BR"
  observation$sample_type <- NA
  observation$sample_number <- NA
  observation$sample_area <- NA
  write.table(
    observation, 
    file = paste("data/raw/", file_name[i], "-observation.csv", sep = ""), 
    sep = "\t", fileEncoding = "UTF-8", row.names = FALSE)
  
  layer <- db[[i]][, cols_layer]
  layer <- layer[with(layer, order(Código.PA, Código.Horizonte)), ]
  write.table(
    layer, 
    file = paste("data/raw/", file_name[i], "-layer.csv", sep = ""), 
    sep = "\t", fileEncoding = "UTF-8", row.names = FALSE)
})

# Identificar trabalhos do Rio Grande do Sul

tit <- "VI Reunião de Correlação, Classificação e Aplicação de Levantamentos de Solos RS/SC/PR"
idx <- lapply(db, function (x) which(x$Título.do.Trabalho == tit))
file_name[which(sapply(idx, length) > 0)]

tit <- "LEVANTAMENTO DETALHADO DE ÁREA PILOTO PARA CONSERVAÇÃO DE SOLOS NO MUNICÍPIO DE IBIRUBÁ, RS."
idx <- lapply(db, function (x) which(x$Título.do.Trabalho == tit))
file_name[which(sapply(idx, length) > 0)]

tit <- "PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 33."
idx <- lapply(db, function (x) which(x$Título.do.Trabalho == tit))
file_name[which(sapply(idx, length) > 0)]

tit <- "Estudo expedito de solos do estado do Rio Grande do Sul e parte de Santa Catarina, para fins de classificação, correlação e legenda preliminar."
idx <- lapply(db, function (x) which(x$Título.do.Trabalho == tit))
file_name[which(sapply(idx, length) > 0)]

tit <- "XIV Congresso Brasileiro de Ciência do Solo."
idx <- lapply(db, function (x) which(x$Título.do.Trabalho == tit))
file_name[which(sapply(idx, length) > 0)]

tit <- "LEVANTAMENTO DE RECONHECIMENTO DO SOLOS DO ESTADO DO RIO GRANDE DO SUL. Boletim Técnico n°30"
idx <- lapply(db, function (x) which(x$Título.do.Trabalho == tit))
file_name[which(sapply(idx, length) > 0)]

tit <- "LEVANTAMENTO DE RECONHECIMENTO DOS SOLOS DO ESTADO DO RIO GRANDE DO SUL. PRIMEIRA ETAPA, PLANALTO RIO-GRANDENSE"
idx <- lapply(db, function (x) which(x$Título.do.Trabalho == tit))
file_name[which(sapply(idx, length) > 0)]

# Carregar dados compilados por pesquisadores da Esalq
esalq <- read.csv(
  "data/raw/fe0003/esalq.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

# Atribuir informação da UF para dados da Esalq
states <- raster::shapefile("data/gis/states.shp")
tmp <- esalq[, c("latitude", "longitude")]
sp::coordinates(tmp) <- c("latitude", "longitude")[2:1]
sp::proj4string(tmp) <- sp::proj4string(states)
tmp <- sp::over(tmp, states)
esalq$UF <- tmp[, 1]

# Salvar aquivo da esalq com dados do RS
idx <- which(esalq$UF == "RS")
tmp <- esalq[idx, ]
write.table(
  tmp, file = "data/raw/esalq-rs.csv", sep = "\t", fileEncoding = "UTF-8", row.names = FALSE)

