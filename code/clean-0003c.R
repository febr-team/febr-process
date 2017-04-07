# Contribuição 0003
# Responsável: Sistema de Informação de Solos Brasileiros
# Instituição: Embrapa Informática Agropecuária / Embrapa Solos

# Nesse script são processados todos os perfis que não possuem o código de identificação do perfil 
# [Código PA] no SISB, independente de possuírem qualquer dados de ferro. Ao invés dos dados originais, são
# usados os dados pós-processadoss do SISB (versão 2).

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")
source("code/helper-utf8.R", encoding = "UTF-8")

# Carregar dados
db <- read.csv(
  "data/raw/fe0003/embrapa-pos-02.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE,
  encoding = "UTF-8")

# IDENTIFICAÇÃO ###############################################################################################

# Há quatro pefis para os quais o trabalho não possui código. Verifiquei que três deles foram adicionados duas
# vezes no SISB, uma vez com as letras do título todas em caixa alta, a outra com as letras em caixa baixa.
# Quando inseridos em caixa baixa, não foram inseridos dados dos horizontes. O quarto trabalho -- 
# Caracterização dos Solos do Município de Piraí do Sul, PR -- simplesmente não possui qualquer horizonte
# associado.
idx <- which(is.na(db$Código.Trabalho))
db <- db[-idx, ]

# Atribuir unidade federativa com base na unidade federativa dos demais perfis do mesmo trabalho
db$UF[which(db$UF == "")] <- NA_character_
na_uf <- which(is.na(db$UF))
id_code <- db$Código.Trabalho[na_uf]
id_code <- lapply(id_code, function (x) db$UF[which(db$Código.Trabalho %in% x)])
id_code <- lapply(id_code, unique)
id_code <- lapply(id_code, na.exclude)
db$UF[na_uf] <- sapply(id_code, function (x) ifelse(length(x) == 1, x, NA_character_))
db[na_uf, c("Título.do.Trabalho", "UF")]

# Muitos perfis (766) não possuem código identificador [Código PA]. Esses perfis são perfis para os quais não
# foram digitadas quaisquer informações sobre os horizontes no SISB. Para os perfis que são do RADAMBRASIL, 
# aproximadamente 350 perfis, a ausência de dados sobre os horizontes deve-se ao fato de os mesmos terem sido
# compilados de outros trabalhos -- não faria sentido ter os dados duplicados na base de dados. Ainda não sei 
# o que se passa com os demais.
idx <- which(is.na(db$Código.PA));length(idx)
db$Código.PA[idx] <- "unknown"

# Ao contrário do que informa o SISB, não há 9119 perfis do solo, mas sim 8889 -- removidos os quatro 
# trabalhos citados acima. Onde estão esses 230 perfis do solo?
cols <- c(
  "Código.PA", "Número.PA", "Número.de.Campo", "Localização.descritiva", "Título.do.Trabalho",
  "Data.da.Coleta")
nrow(unique(db[, cols]))

# Criar identificador único para os perfis do solo. Para isso são usadas diversas informações sobre os perfis.
db$id <- 1
cols <- c(cols, "id")

# Código.PA
i <- 1
pf <- db[, cols[c(i, length(cols))]]
pf <- unique(pf);nrow(pf)
pf$profile_id <- rnorm(nrow(pf), mean = 100)
db <- merge(x = pf, y = db, by = cols[i], all = TRUE);dim(db)
db <- db[, !colnames(db) %in% c("id.x", "id.y")]
db$id <- 1

# Número.PA
i <- 2
pf <- db[, cols[c(i, length(cols))]]
pf <- unique(pf);nrow(pf)
pf$profile_id <- rnorm(nrow(pf), mean = 100)
db <- merge(x = pf, y = db, by = cols[i], all = TRUE);dim(db)
db$profile_id <- db$profile_id.x + db$profile_id.y;length(unique(db$profile_id))
db <- db[, !colnames(db) %in% c("profile_id.x", "profile_id.y", "id.x", "id.y")]
db$id <- 1

# Número.de.Campo
i <- 3
pf <- db[, cols[c(i, length(cols))]]
pf <- unique(pf);nrow(pf)
pf$profile_id <- rnorm(nrow(pf), mean = 100)
db <- merge(x = pf, y = db, by = cols[i], all = TRUE);dim(db)
db$profile_id <- db$profile_id.x + db$profile_id.y;length(unique(db$profile_id))
db <- db[, !colnames(db) %in% c("profile_id.x", "profile_id.y", "id.x", "id.y")]
db$id <- 1

# Localização.descritiva
i <- 4
pf <- db[, cols[c(i, length(cols))]]
pf <- unique(pf);nrow(pf)
pf$profile_id <- rnorm(nrow(pf), mean = 100)
db <- merge(x = pf, y = db, by = cols[i], all = TRUE);dim(db)
db$profile_id <- db$profile_id.x + db$profile_id.y;length(unique(db$profile_id))
db <- db[, !colnames(db) %in% c("profile_id.x", "profile_id.y", "id.x", "id.y")]
db$id <- 1

# Título.do.Trabalho
i <- 5
pf <- db[, cols[c(i, length(cols))]]
pf <- unique(pf);nrow(pf)
pf$profile_id <- rnorm(nrow(pf), mean = 100)
db <- merge(x = pf, y = db, by = cols[i], all = TRUE);dim(db)
db$profile_id <- db$profile_id.x + db$profile_id.y;length(unique(db$profile_id))
db <- db[, !colnames(db) %in% c("profile_id.x", "profile_id.y", "id.x", "id.y")]
db$id <- 1

# Data.da.Coleta
i <- 6
pf <- db[, cols[c(i, length(cols))]]
pf <- unique(pf);nrow(pf)
pf$profile_id <- rnorm(nrow(pf), mean = 100)
db <- merge(x = pf, y = db, by = cols[i], all = TRUE);dim(db)
db$profile_id <- db$profile_id.x + db$profile_id.y;length(unique(db$profile_id))
db <- db[, !colnames(db) %in% c("profile_id.x", "profile_id.y", "id.x", "id.y")]

# Processar identificador individual.
# Salvar arquivo temporário para verificar a estrutura dos dados.
db$profile_id <- as.character(round(db$profile_id * 10e8))
write.csv(db[db$Código.PA == "unknown", ], "tmp.csv")

# Levantamento de reconhecimento dos solos da região sul-sudeste do  Estado do Amazonas-IPAAM.
# n = 11
# São 11 perfis para os quais não foram inseridas informações sobre os horizontes. É possível que esses 
# perfis não possuam horizontes associados. Não foi possível verificar pois não tenho acesso ao original. Os
# mesmos podem ser mantidos. Todos possuem coordenadas. Cinco deles parecem ter sido compilados de outras 
# fontes -- RADAMBRASIL.
tit <- " Levantamento de reconhecimento dos solos da região sul-sudeste do  Estado do Amazonas-IPAAM."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))

# Levantamento do Estado do Rio de Janeiro
# n = 10
# Parece que todos os perfis existem. Contudo, isso só pode ser verificado via acesso ao original.
tit <- " Levantamento do Estado do Rio de Janeiro"
idx <- which(db$Título.do.Trabalho == tit)
unique(db$Data.da.Coleta[idx])

# 2nd Global Workshop on Digital Soil Mapping for Regions and Countries with Sparse Soil Data 
# Infrastructures - Tour Guide
# n = 3
# Os três perfis não existem. Eles foram inseridos para teste. Os três podem ser deletados.
tit <- "2nd Global Workshop on Digital Soil Mapping for Regions and Countries with Sparse Soil Data Infrastructures - Tour Guide"
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# 39 - Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das 
# Terras da Região Geoeconômica de Brasília - Minas Gerais.
# n = 2
# Talvez os dados dos horizontes não tenham sido digitados para esses dois perfis. É preciso ter acesso à 
# fonte para verificar isso.
tit <- "39 - Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras da Região Geoeconômica de Brasília - Minas Gerais."
idx <- which(db$Título.do.Trabalho == tit)
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db$Número.PA[idx]))

# Anais da III Reunião  de Classificação, Correlação de Solos e Interpretação de Aptidão Agrícola
# n = 6
# Conferindo o trabalho original, pode-se inferir que os seis perfis não existem.
tit <- "Anais da III Reunião  de Classificação, Correlação de Solos e Interpretação de Aptidão Agrícola."
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Caracterização dos Solos do Município de Castro, PR
# n = 1
# Os dados do trabalho foram digitados, tendo o perfil verificado sido usado para teste. O mesmo pode ser 
# removido.
tit <- "Caracterização dos Solos do Município de Castro, PR"
idx <- which(db$Título.do.Trabalho == tit)
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db$Número.PA[idx]))
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Caracterização e Classificação de Solos do Terciário no Nordeste do Estado do Pará.
# n = 2
# Os dados dos horizontes de dois perfis não foram inseridos no SISB. Sem acesso ao original, não há o que 
# fazer.
tit <- "Caracterização e Classificação de Solos do Terciário no Nordeste do Estado do Pará."
idx <- which(db$Título.do.Trabalho == tit)
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db$Número.PA[idx]))

# Estudo Expedito de Solos da Região do Alto Paranaíba, Para Fins de Classificação, Correlação e Legenda 
# Preliminar.
# n = 18
# Todos os 18 perfis não possuem resultados analíticos, conforme descrito no item Informações Complementares.
# Os mesmos são mantidos por conter outras informações.
tit <- "Estudo Expedito de Solos da Região do Alto Paranaíba, Para Fins de Classificação, Correlação e Legenda Preliminar."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db$Informações.Complementares[idx]

# Estudo Expedito de Solos da Região Sul de Minas Gerais, Partes do Alto São Francisco e Campos das Vertestes, 
# Para Fins de Classificação, Correlação e Legenda Preliminar.
# n = 35
# os perfis em questão não possuem dados analíticos para horizontes, mas apenas dados locais.
tit <- "Estudo Expedito de Solos da Região Sul de Minas Gerais, Partes do Alto São Francisco e Campos das Vertestes, Para Fins de Classificação, Correlação e Legenda Preliminar."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit)
db$Código.PA[idx]

# Estudo Expedito de Solos do Estado de Santa Catarina, para fins de Classificação, Correlação e Legenda 
# Preliminar.
# n = 18
# Os perfis em questão não possuem dados analíticos.
tit <- "Estudo Expedito de Solos do Estado de Santa Catarina, para fins de Classificação, Correlação e Legenda Preliminar."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))

# Estudo Expedito de Solos do Trecho Cuiabá-Aripuanã, MT, Para Fins de Correlação e Classificação
# n = 19
# Os perfis em questão não possuem dados analíticos.
tit <- "Estudo Expedito de Solos do Trecho Cuiabá-Aripuanã, MT, Para Fins de Correlação e Classificação"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))

# Estudo Expedito de Solos no Estado do Maranhão para Fins de Classificação, Correlação e Legenda Preliminar.
# n = 16
# O original não está disponível, mas suponho que não haja dados analíticos para os perfis.
tit <- "Estudo Expedito de Solos no Estado do Maranhão para Fins de Classificação, Correlação e Legenda Preliminar."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))

# Estudo Expedito de Solos no Estado do Paraná para fins de Classificação e Correlação.
# n = 150
# Suponho que não haja dados analíticos, com excessão de alguns poucos perfis já inseridos no SISB.
tit <- "Estudo Expedito de Solos no Estado do Paraná para fins de Classificação e Correlação."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))

# Estudo Expedito de Solos no estado do Piauí Para Fins de Classificação, Correlação e Legenda Preliminar
# n = 9
# Parece-me que não haja dados para esses perfis.
tit <- "Estudo Expedito de Solos no estado do Piauí Para Fins de Classificação, Correlação e Legenda Preliminar"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))

# Estudo Expedito de Solos no Trecho Itaituba - Estreito da Rodoviária Transamazônica para Fins de 
# Classificação e Correlação (Agosto de 1972). 
# n = 51
# Parece estar OK.
tit <- "Estudo Expedito de Solos no Trecho Itaituba - Estreito da Rodoviária Transamazônica para Fins de Classificação e Correlação (Agosto de 1972). "
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit)
db[idx, c("Número.PA", "Símbolo.Horizonte", "Ataque.sulfúrico...Fe2O3")]

# Inventário das terras em microbacias hidrográficas, 12 Microbacia: Capelinha (Ipira,SC).
# n = 1
# Os perfis deste trabalho já foram inseridos no SISB usando um nome diferente o perfil.
# O referido perfil é eliminado.
tit <- "Inventário das terras em microbacias hidrográficas, 12\nMicrobacia: Capelinha (Ipira,SC)."
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento de Reconhecimento de Alta Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras das 
# Quadrículas de Silva Jardim e Rio das Ostras, Estado do Rio de Janeiro
# n = 2
# Parece que há perfis sem dados analíticos na origem.
tit <- "Levantamento de Reconhecimento de Alta Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras das Quadrículas de Silva Jardim e Rio das Ostras, Estado do Rio de Janeiro"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))

# Levantamento de reconhecimento de baixa intensidade dos solos e avaliação da aptidão agrícola das terras de
# área piloto no município de Barreirinha - Estado do Amazonas.
# n <- 14
# O levantamento foi inserido duas vezes no SISB, a primeira delas usando caixa alta para o título, para o
# qual foi inserido apenas um perfil. Esse perfil também foi inserido na segunda vez que o trabalho foi 
# inserido, portanto pode ser deletado. Os dados analíticos existem, mas não foram digitados. Talvez tenham 
# sido aproveitados em outro levantamento.
tit <- "Levantamento de reconhecimento de baixa intensidade dos solos e avaliação da aptidão agrícola das terras de área piloto no município de Barreirinha - Estado do Amazonas."
idx <- which(db$Título.do.Trabalho == tit)
length(idx)
tit <- "Levantamento de Reconhecimento de Baixa Intensidade dos Solos e Avaliação da Aptidão Agricola das Terras de Área Piloto no Município de Barreirinha - Estado do Amazonas"
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento de Reconhecimento de Média Intensidade do Solos da Região dos Tabuleiros Costeiros e da Baixada
# Litorânea do Estado de Sergipe
# n = 2
# Os dois perfis foram compilados de outros tabalhos. Segundo o documento original, existem 113 perfis, onde
# apenas 39 foram coletados. O perfil 67 na verdade é o perfil 71. Mas os dados já foram digitados. Os dados
# do perfil 77 também já foram digitados. Os dois podem ser deletados.
tit <- "Levantamento de Reconhecimento de Média Intensidade do Solos da Região dos Tabuleiros Costeiros e da Baixada Litorânea do Estado de Sergipe"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "PERFIL 77")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# LEVANTAMENTO DE RECONHECIMENTO DE MÉDIA INTENSIDADE DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DA 
# ÁREA DO PÓLO PRÉ-AMAZÔNIA MARANHENSE
# n = 1
# Um horizonte extra foi inserido por engano. Pode ser deletado.
tit <- "LEVANTAMENTO DE RECONHECIMENTO DE MÉDIA INTENSIDADE DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DA ÁREA DO PÓLO PRÉ-AMAZÔNIA MARANHENSE"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "AMOSTRA EX. 52")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras da 
# Área do Pólo Tapajós.
# n = 2
# Horizontes adicionados por engano. Podem ser deletados.
tit <- "Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras da Área do Pólo Tapajós."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "Extra 30")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "Extra 22")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# LEVANTAMENTO DE RECONHECIMENTO DE MÉDIA INTENSIDADE DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DA
# MARGEM DIREITA DO RIO PARANÃ - ESTADO DE GOIÁS
# n = 1
# Horizonte adicionado erroneamente. Pode ser deletado.
tit <- "LEVANTAMENTO DE RECONHECIMENTO DE MÉDIA INTENSIDADE DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DA MARGEM DIREITA DO RIO PARANÃ - ESTADO DE GOIÁS"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "FERTILIDADE N53")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras de 
# 21.000 hectares no Município de Tefé, Amazonas.
# n = 13
# Segundo o original, há apenas 11 perfis. Aqui temos o perfil 5 repetido duas vezes. Entratanto, os dados 
# analíticos não foram inseridos. Talvez tenham sido usados em outro estudo. Deletar os dois registros 
# repetidos do perfil 5.
tit <- "Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras de 21.000 hectares no Município de Tefé, Amazonas."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown" & db$Número.de.Campo == "EMADE-AM-4")
db <- db[-idx[-1], ]

# Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras em 
# 100.000 Hectares da Gleba Machadinho, no Município de Ariquemes, Rondônia.
# n = 1
# Horizonte digitado por engano. Deletar.
tit <- "Levantamento de Reconhecimento de Média Intensidade dos Solos e Avaliação da Aptidão Agrícola das Terras em 100.000 Hectares da Gleba Machadinho, no Município de Ariquemes, Rondônia."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "Perfil 19")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# LEVANTAMENTO DE RECONHECIMENTO DOS SOLOS DO DISTRITO FEDERAL
# n = 2
# Os dois horizontes foram inseridos sem necessidade. Deletar.
tit <- "LEVANTAMENTO DE RECONHECIMENTO DOS SOLOS DO DISTRITO FEDERAL"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "PERFIL DF 49")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "PERFIL DF 7")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento de Reconhecimento Dos Solos do Estado do Paraná
# n = 1
# Horizonte inserido sem necessidade. Deletar.
tit <- "Levantamento de Reconhecimento Dos Solos do Estado do Paraná"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "Perfil nº82")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento de reconhecimento dos solos do sul do estado de Mato Grosso
# n = 2
# Os perfis existem, mas os dados não foram digitados. talvez estejam em outro trabalho. Há mais perfis
# não digitados nesse trabalho.
tit <- "Levantamento de reconhecimento dos solos do sul do estado de Mato Grosso"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "04")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "Perfil 07")
db$Símbolo.Horizonte[idx]

# LEVANTAMENTO DE RECONHECIMENTO DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DE UMA ÁREA DE 
# COLONIZAÇÃO NO MUNICÍPIO DE URUCARÁ, ESTADO DO AMAZONAS.
# n = 1
# Horizonte insirido sem necessidade. Deletar.
tit <- "LEVANTAMENTO DE RECONHECIMENTO DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DE UMA ÁREA DE COLONIZAÇÃO NO MUNICÍPIO DE URUCARÁ, ESTADO DO AMAZONAS."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "PERFIL 6")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento Exploratório - Reconhecimento de Solos da Margem Direita do Rio São Franciso. Estado da Bahia - 
# Volume I.
# n = 1
# Horizonte desnecessário. Deletar.
tit <- "Levantamento Exploratório - Reconhecimento de Solos da Margem Direita do Rio São Franciso. Estado da Bahia - Volume I."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "174")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# LEVANTAMENTO EXPLORATÓRIO - RECONHECIMENTO DE SOLOS DO ESTADO DE SERGIPE.
# n = 2
# Os dois horizontes foram digitados sem necessidade. Deletar.
tit <- "LEVANTAMENTO EXPLORATÓRIO - RECONHECIMENTO DE SOLOS DO ESTADO DE SERGIPE."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "PERFIL 21")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "PERFIL 17")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento exploratório - Reconhecimento de solos do Estado do Ceará 
# n = 2
# Dois horizontes desnecessários. Deletar.
tit <- "Levantamento exploratório - Reconhecimento de solos do Estado do Ceará "
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "P71")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "P62")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# LEVANTAMENTO EXPLORATÓRIO DOS SOLOS QUE OCORREM AO LONGO DA RODOVIA MANAUS - PORTO VELHO
# n = 1
# Horizonte desnecessário. Deletar.
tit <- "LEVANTAMENTO EXPLORATÓRIO DOS SOLOS QUE OCORREM AO LONGO DA RODOVIA MANAUS - PORTO VELHO"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "6-EXTRA")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# Levantamento semidetalhado de solos: região da campanha - Folha Palomas, Estado do Rio Grande do Sul
# n = 1
# Nenhum dado foi inserido para esse trabalho. Deletar.
tit <- "Levantamento semidetalhado de solos: região da campanha - Folha Palomas, Estado do Rio Grande do Sul"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP.
# n = 1
# Não consegui os dados originais. Talvez exista.
tit <- "LEVANTAMENTO SEMIDETALHADO DOS SOLOS DA FAZENDA CANCHIM SÃO CARLOS - SP."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "E58")
db$Símbolo.Horizonte[idx]

# Levantamento Semidetalhado dos Solos de Áreas do Ministério da Agricultura no Distrito Federal.
# n = 2 
# Horizontes desnecessários. Deletar.
tit <- "Levantamento Semidetalhado dos Solos de Áreas do Ministério da Agricultura no Distrito Federal."
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "8")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Número.PA == "Ficha de Campo3")
db$Símbolo.Horizonte[idx]
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]

# PEDOGÊNESE DE ESPODOSSOLOS EM AMBIENTES DA FORMAÇÃO BARREIRAS E DE RESTINGA DO SUL DA BAHIA
# n = 1
# Nenhum dado foi inserido para esse trabalho. Deletar.
tit <- "PEDOGÊNESE DE ESPODOSSOLOS EM AMBIENTES DA FORMAÇÃO BARREIRAS E DE RESTINGA DO SUL DA BAHIA"
nrow(db[db$Título.do.Trabalho == tit, ])
length(unique(db$profile_id[db$Título.do.Trabalho == tit]))
sort(unique(db[db$Título.do.Trabalho == tit, "Número.PA"]))
idx <- which(db$Título.do.Trabalho == tit & db$Código.PA == "unknown")
db <- db[-idx, ]



# i <- agrep("PODZÓLICO VERMELHO-AMARELO Tb Álico A moderado textura argilosa/ muito argilosa fase floresta tropical subperenifólia relevo plano", 
#       db$Classificação.Original, ignore.case = T)
# 
# db$Título.do.Trabalho[i]
# 
# db$Ataque.sulfúrico...Fe2O3[14301]
# 
# 
# 
# # Manter apenas registros sem 'Código.PA'
# db <- db[is.na(db$Código.PA), ];nrow(db)
# db$Número.PA[which(db$Número.PA == "")] <- NA_character_
# tmp <- 
#   db[, c("Número.PA", "Número.de.Campo", "Localização.descritiva", "Classificação.Original", 
#          "Título.do.Trabalho", "Data.da.Coleta", "UF", "Município")]
# tmp <- unique(tmp)
# str(tmp)
# write.csv(tmp, "tmp.csv")
# 
# db[rownames(tmp), "Código.PA"]
# 
# # PERFIS ######################################################################################################
# 
# # Definir as colunas necessárias
# lat_cols <- colnames(db)[grep("Lat", colnames(db))]
# long_cols <- colnames(db)[grep("Long", colnames(db))]
# pf <- db[, c(
#   "Código.Trabalho", "Localização.descritiva", "Referência.Bibliográfica",
#   "Código.PA", "Número.PA", "Data.da.Coleta", "Título.do.Trabalho", "Ano.de.Publicação", "Tipo.de.Publicação",
#   "Número", "Datum", "Northing", "Easting", lat_cols, long_cols, "UF", "Município",
#   "Classificação.Original", "Classe.de.Solos.Nível.3", "X1ª.Ocorrência", "Uso.Atual", "Litologia")]
# 
# nrow(pf)
