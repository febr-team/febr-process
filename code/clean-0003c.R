# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")

# Carregar dados
db <- read.csv(
  "data/raw/fe0003/embrapa.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, 
  encoding = "UTF-8")

# Identificar linhas e colunas contendo dados de ferro
id_col <- colnames(db)[grep("Fe", colnames(db))] 
id_col
id_col <- id_col[-2]
idx <- which(!is.na(db[, id_col]), arr.ind = TRUE)
id_col <- id_col[unique(idx[, 2])]
id_row <- unique(idx[, 1])
db <- db[-id_row, ]

# Atribuir unidade federativa com base na unidade federativa dos demais perfis do mesmo trabalho
db$UF[which(db$UF == "")] <- NA_character_
na_uf <- which(is.na(db$UF))
id_code <- db$Código.Trabalho[na_uf]
id_code <- lapply(id_code, function (x) db$UF[which(db$Código.Trabalho %in% x)])
id_code <- lapply(id_code, unique)
id_code <- lapply(id_code, na.exclude)
db$UF[na_uf] <- sapply(id_code, function (x) ifelse(length(x) == 1, x, NA_character_))
db[na_uf, c("Título.do.Trabalho", "UF")]

# Atribuir unidade federativa e município
# Alguns perfis permanecem sem informação, principalmente os do RADAMBRASIL.
na_uf <- which(is.na(db$UF));length(na_uf)
#
i <- 1
db[na_uf[i], c("Título.do.Trabalho", "Referência.Bibliográfica", "Município")]
db[na_uf[i], "UF"] <- "MG"
#
i <- 2
db[na_uf[i], c("Título.do.Trabalho", "Referência.Bibliográfica", "Município")]
db[na_uf[i], "UF"] <- "PR"
db[na_uf[i], "Município"] <- "Piraí do Sul"
#
i <- 4
db[na_uf[i], c("Título.do.Trabalho", "Referência.Bibliográfica", "Município")]
db[na_uf[i], "UF"] <- "AM"
db[na_uf[i], "Município"] <- "Careiro"
#
i <- 8
db[na_uf[i], c("Título.do.Trabalho", "Referência.Bibliográfica", "Município")]
db[na_uf[i], "UF"] <- "GO"
#
i <- 18
db[na_uf[i], c("Título.do.Trabalho", "Referência.Bibliográfica", "Município")]
db[na_uf[i], "UF"] <- "AM"
db[na_uf[i], "Município"] <- "Barreirinha"

# PERFIS ######################################################################################################

# Definir as colunas necessárias
lat_cols <- colnames(db)[grep("Lat", colnames(db))]
long_cols <- colnames(db)[grep("Long", colnames(db))]
pf <- db[, c(
  "Código.Trabalho", "Localização.descritiva", "Referência.Bibliográfica",
  "Código.PA", "Número.PA", "Data.da.Coleta", "Título.do.Trabalho", "Ano.de.Publicação", "Tipo.de.Publicação",
  "Número", "Datum", "Northing", "Easting", lat_cols, long_cols, "UF", "Município",
  "Classificação.Original", "Classe.de.Solos.Nível.3", "X1ª.Ocorrência", "Uso.Atual", "Litologia")]
pf <- pf[!duplicated(pf$Código.PA), ]

# PERFIS: Data de observação ----
pf$observation_date <- gsub("/", "-", pf$Data.da.Coleta)
pf$observation_date <- gsub("--", NA_character_, pf$observation_date)

# PERFIS: Uso da terra ----
pf$land_use <- NA_character_
out <- guessClass(pf$Uso.Atual, keywords = keys, max.distance = 0.1)

# Elevada probabilidade
p_max <- 0.7
p_high <- which(apply(out, 2, function (x) max(x) > p_max))
nam <- names(keys)[apply(out[, p_high], 2, which.max)]
pf$land_use[p_high] <- nam

# Moderada probabilidade
p_mod <- 
  which(
    apply(out, 2, function (x) {
      y <- sort(x, decreasing = TRUE)[1:2]
      y[1] <= p_max && y[2] >= (1 - p_max)
    })
  )
nam <- apply(out[, p_mod], 2, function (x) sort(x, decreasing = TRUE, index.return = TRUE)$ix[1:2])
nam <- matrix(names(keys)[nam], nrow = 2)
pf$land_use[p_mod] <-
  apply(nam, 2, function (x) {
    if (sum(c("AGRICULTURA", "CRIAÇÃO ANIMAL") %in% x) == 2 || 
        sum(c("AGRICULTURA", "SILVICULTURA") %in% x) == 2 ||
        sum(c("CRIAÇÃO ANIMAL", "SILVICULTURA") %in% x) == 2) {
      "CULTIVO MISTO"
    } else if (any(x == "AGRICULTURA")) {
      "AGRICULTURA"
    } else if (any(x == "CRIAÇÃO ANIMAL")) {
      "CRIAÇÃO ANIMAL"
    } else if (any(x == "SILVICULTURA")) {
      "SILVICULTURA"
    } else if (any(x == "URBANO")) {
      "URBANO"
    } else {
      paste(x[1], "/", x[2])
    }
  })
id <- sample(1:length(pf$land_use), 10)
pf[id, c("Uso.Atual", "land_use")]

# PERFIS: Litologia ----
pf$litology <- ifelse(pf$Litologia == "", NA_character_, pf$Litologia)

# PERFIS: Coordenadas geográficas ----

# Carregar shapefile com os limites do estados brasileiros
states <- raster::shapefile("data/gis/states.shp")
bb_br <- sp::bbox(states)
bb_br[, 1] <- abs(floor(bb_br[, 1]))
bb_br[, 2] <- abs(ceiling(bb_br[, 2]))

# Conferir se exitem coordenadas negativas. Em princípio, todas as coordenadas deveriam ser positivas porque
# cada registro possui informação sobre o hemisfério em que a observação foi realizada.
# Latitude
idx <- apply(pf[, lat_cols[1:3]], 2, function (x) x < 0)
any(idx, na.rm = TRUE)
# Longitude
idx <- apply(pf[, long_cols[1:3]], 2, function (x) x < 0)
any(idx, na.rm = TRUE)

# Conferir se o hemisfério especificado está correto. Em princípio, todos os registros deveriam estar à
# oeste the Greenwich, a maioria deles ao sul to Equador. Isso porque apenas quatro estados brasileiros
# possuem (parte de) seus territórios ao norte do Equador. São eles: Amazonas (AM), Amapá (AP), Pará (PA) e
# Roraima (RR).
# Longitude
# Inúmeros registros possuem 'Leste' como valor para o hemisfério, dado este completamente equivocado. Em
# todos os registros o hemisfério é definido como sendo 'Oeste'. Curiosamente a maioria é no RJ e SP.
idx <- which(pf[, long_cols[4]] == "Leste")
pf[idx, c(long_cols[4], "Código.PA", "UF")]
pf[idx, long_cols[4]] <- "Oeste"
pf[, long_cols[4]] <- ifelse(pf[, long_cols[4]] == "", NA_character_, pf[, long_cols[4]])
# Latitude
# Inúmeros registros possuem 'Norte' como hemisfério, apesar de não terem sido observados nos estados
# do AM, AP, PA ou RR. O valor do hemisfério desses registros é alterado para 'Sul'. A maioria no RJ.
idx <- which(pf[, lat_cols[4]] == "Norte" & !sapply(pf$UF, function (x) x %in% c("AP", "AM", "PA", "RR")))
pf[idx, c(lat_cols[4], "Código.PA", "UF")]
pf[idx, lat_cols[4]] <- "Sul"
pf[, lat_cols[4]] <- ifelse(pf[, lat_cols[4]] == "", NA_character_, pf[, lat_cols[4]])

# Conferir se há valores de latitude e longitude maiores do que os maiores valores absolutos de latitude
# e longitude do território brasileiro. Em princípio, todos os valores de latitude deveriam ser menores do que 
# aproximadamente 34º S, ou seja, a latitude mais ao sul do teritório brasileiro. Da mesma forma, todos os
# valores de longitude deveriam ser inferiores a aproximadamente 74º O.
# Latitude
# Há apenas um erro, no AM, que pode ser corrigido dividindo o valor de latitude por 10.
idx <- which(pf[, lat_cols[1]] > bb_br[2, 1])
length(idx) != 0
pf[idx, c(lat_cols, long_cols[1], "UF", "Código.PA", "Título.do.Trabalho")]
pf[idx, lat_cols[1]] <- pf[idx, lat_cols[1]] / 10
states[states@data$uf == pf[idx, "UF"], ]@bbox
# Longitude
# Não há erros para longitude.
# Também é sabido que, em princípio, todos os valores de longitude deveriam ser maiores do que aproximadamente
# 28º O. Um registro apresenta erro, no PI. Pode ser um erro de digitação, 41° ao invés de 1°.
idx <- which(pf[, long_cols[1]] < bb_br[1, 2])
length(idx) != 0
pf[idx, c(long_cols, lat_cols, "UF", "Código.PA", "Localização.descritiva")]
ibge <- getCity(pf[idx, "Município"])@bbox
pf[idx, long_cols[1]] <- 41

# Conferir se todos os registros com coordenadas possuem dado sobre o hemisfério.
# Em princípio, todos os registros deveriam estar a oeste de Greenwich (Oeste), a maioria deles ao sul do 
# Equador (Sul). Quando um registro cai em um dos quatro estados brasileiros com território no hemisfério 
# norte (AM, AP, PA e RR), eu confiro se a latitude é maior do que a maior latitude norte do território 
# brasileiro. Se essa condição for atendida, então o registro em questão deveria estar no hemisfério sul. Para
# os demais casos é preciso verificar outras informações.
# Latitude
idx <- which(is.na(pf[, lat_cols[4]]) & !is.na(pf[, lat_cols[1]]))
pf[idx, c(lat_cols[1], "UF")]
pf[idx, lat_cols[4]] <- 
  sapply(idx, function (i) {
    if (pf[i, "UF"] %in% c("AP", "AM", "PA", "RR")) {
      if (pf[i, lat_cols[1]] > bb_br[2, 2]) {
        "Sul"
      } else {
        NA
      }
    } else {
      "Sul"
    }
  })
pf[idx, c(lat_cols, "UF")]
# Três registros permanecem sem dado sobre o hemisfério.
idx <- which(is.na(pf[, lat_cols[4]]) & !is.na(pf[, lat_cols[1]]))
pf[idx, c("Município", lat_cols)]
pf[idx[1], c("Município", "Localização.descritiva")]
pf[idx[1], lat_cols[4]] <- "Sul"
getCity(pf[idx[2], "Município"])@bbox
pf[idx[2], lat_cols[4]] <- "Sul"
getCity(pf[idx[3], "Município"])@bbox
pf[idx[3], lat_cols[4]] <- "Sul"
# Longitude
idx <- which(is.na(pf[, long_cols[4]]) & !is.na(pf[, long_cols[1]]))
pf[idx, c(long_cols, "UF")]
pf[idx, long_cols[4]] <- "Oeste"

# Conferir os valores de latitude no hemisfério norte.
# Em princípio, todos os valores de latitude dos registros no hemisfério norte deveriam ser menores do que
# 6º N. Eu encontro um único registro, que corresponde à observação feita no município de Humaitá (AM),
# localizado no hemisfério sul.
idx <- which(pf[, lat_cols[4]] == "Norte" & pf[, lat_cols[1]] > bb_br[2, 2])
length(idx) != 0
pf[idx, c(lat_cols, "UF", "Código.PA", "Município")]
pf[idx, lat_cols[4]] <- "Sul"

# Conferir se há valores de minutos e segundos superiores a 60.
