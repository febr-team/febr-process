# Contribuição 0003
# Responsável: Sistema de Informação de Solos Brasileiros
# Instituição: Embrapa Informática Agropecuária / Embrapa Solos

# Com esse script são processados todos os perfis que contenham, obrigatoriamente, o código de identificação 
# do perfil [Código PA] no SISB, mas que não possuem qualquer dados de ferro.

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")
source("code/helper-utf8.R", encoding = "UTF-8")

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
# Somente a coluna dos segundos apresenta erro. Os valores são divididos por 10 iterativamente até que 
# sejam < 60.
# Latitude
idx <- apply(pf[, lat_cols[2:3]], 2, function (x) x > 60)
any(idx, na.rm = TRUE)
idx <- which(idx, arr.ind = TRUE)
any(duplicated(idx[, 1])) # não há registros duplicados
pf[idx[, 1], c(lat_cols, long_cols)]
tmp <- pf[, lat_cols[2:3]][idx]
tmp # minutos e segundos > 60
for (i in 1:length(tmp)) {
  divide <- TRUE
  while (divide) {
    tmp[i] <- tmp[i] / 10
    divide <- tmp[i] > 60
  }
}
pf[, lat_cols[2:3]][idx] <- tmp
# Longitude
# Agora nós temos as colunas de minutos e segundos com erro. Os valores são divididos por 10 iterativamente.
idx <- apply(pf[, long_cols[2:3]], 2, function (x) x > 60)
any(idx, na.rm = TRUE)
idx <- which(idx, arr.ind = TRUE)
any(duplicated(idx[, 1])) # não há registros duplicados
pf[idx[, 1], c(lat_cols, long_cols)]
tmp <- pf[, long_cols[2:3]][idx]
tmp # minutos e segundos > 60
for (i in 1:length(tmp)) {
  divide <- TRUE
  while (divide) {
    tmp[i] <- tmp[i] / 10
    divide <- tmp[i] > 60
  }
}
pf[, long_cols[2:3]][idx] <- tmp

# Transformar coordenadas em graus, minutos e segundos para graus decimais
# Latitude
pf$y_coord <- dms2dd(x = pf[, lat_cols], type = "lat")
head(pf[, c(lat_cols[-3], "y_coord")], 10)
# Longitude
pf$x_coord <- dms2dd(x = pf[, long_cols], type = "long")
head(pf[, c(long_cols[-3], "x_coord")], 10)

# Conferir se todos os registros estão dentro do território brasileiro.
# São 24 perfis com coordenadas fora do território brasileiro.
idx <- which(!is.na(pf$x_coord))
tmp <- pf[idx, c("x_coord", "y_coord")]
sp::coordinates(tmp) <- c("x_coord", "y_coord")
sp::proj4string(tmp) <- sp::proj4string(states)
tmp <- sp::over(tmp, states)
sum(is.na(tmp)) # número de registros fora do território brasileiro
sp::plot(states)
idx_out <- idx[which(is.na(tmp))] 
idx_out <- idx_out[order(pf[idx_out, "Município"])]
points(pf[idx_out, c("x_coord", "y_coord")], col = "red")
summary(as.factor(pf[idx_out, "Município"]))
# São Gabriel da Cachoeira
i <- idx_out[which(pf[idx_out, "Município"] == "São Gabriel da Cachoeira")]
pf[i, c("Título.do.Trabalho", "Referência.Bibliográfica", "Número.PA", "y_coord", "x_coord")]
# duckduckgo(pf[i, "Referência.Bibliográfica"][1])
# for (j in 1:length(i)) {
  # a <- pf[i[j], c("y_coord", "x_coord")]
  # a[1] <- a[1] - 1
  # googlemaps(a)
# }
pf[i, "y_coord"] <- pf[i, "y_coord"] - 1
# Demais municípios
i <- idx_out[which(pf[idx_out, "Município"] != "São Gabriel da Cachoeira")]
pf[i, c("Município", "y_coord", "x_coord")]
# Lat e long errados.
j <- 1
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], c("y_coord", "x_coord")] <- c(-11.284399, -37.607083)
# Long -42 ao invés de -40
j <- 2
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] - 2
# Pequena mudança em lat e long
j <- 3
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], c("y_coord", "x_coord")] <- c(-22.939195, -42.239635)
# Long -72° ao invés de -73°.
j <- 4
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 1
# Long -71° ao invés de -72°.
j <- 5
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 1
# Hem Sul ao invés de norte
j <- 6
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] * -1
pf[i[j], lat_cols[4]] <- "Sul"
# Pequeno desvio na longitude
j <- 7
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 0.053
# Lat -21 ao invés de -29
j <- 8
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] + 8
# Alteração de lat e long
j <- 9
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], c("y_coord", "x_coord")] <- c(-4.370446, -60.953022)
# Pequena alteração de long
j <- 10
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 0.03
# Long -59 ao invés de -63
j <- 11
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 4
# Pequena alteração de long
j <- 12
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 0.05
# Pequena alteração de long
j <- 13
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 0.05
# Lat -10 ao invés de -12
j <- 14
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] + 2
# Long -37 ao invés de -36
j <- 15
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] - 1
# Long -39 ao invés de -30
j <- 16
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] - 9
# Long -39 ao invés de -31
j <- 17
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] - 8
# Lat -10 ao invés de -19
j <- 18
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] + 9
# Lat, long e município com erro. O municipio de Búzios costuma aparecer como Rio de Janeiro
j <- 19
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], c("y_coord", "x_coord")] <- c(-22.766719, -41.908848)
pf[i[j], "Município"] <- "Búzios"
# Lat, long e município com erro. O municipio de Búzios costuma aparecer como Rio de Janeiro
j <- 20
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], c("y_coord", "x_coord")] <- c(-22.837606, -43.228308)
# Lat -8 ao invés de - 3
j <- 21
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] - 5
# Pequena alteração de lat e long
j <- 22
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], c("y_coord", "x_coord")] <- c(-13.146777, -62.134124)

# Conferir se todos os registros estão dentro da respectiva unidade federativa.
# A maioria dos erros econtram-se próximos dos limites dos estados.
# Problemas:
# - LEVANTAMENTO EXPLORATÓRIO DOS SOLOS QUE OCORREM AO LONGO DA RODOVIA MANAUS - PORTO VELHO
# - LEVANTAMENTO DE RECONHECIMENTO DE MÉDIA INTENSIDADE DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS
#   DA MARGEM DIREITA DO RIO PARANÃ - ESTADO DE GOIÁS
# - Mapa Esquemático dos Solos das Regiões Norte, Meio-Norte e Centro-Oeste do Brasil
# - Projeto RADAMBRASIL (Amazônia)
idx_uf <- cbind(pf$UF[idx], tmp)
idx_uf <- which(idx_uf[, 1] != idx_uf[, 2])
length(idx_uf) # numero de registros fora da respectiva unidade federativa.
idx_uf <- idx[idx_uf]
points(pf[idx_uf, c("x_coord", "y_coord")], col = "blue")
# Lat -16 ao invés de -17
i <- 1
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] + 1
# Pequena alteração na lat e long
i <- 2
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-12.992200, -46.588977)
# Alteração na lat, long, município e UF
i <- 3
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-12.715185, -46.418354)
pf[idx_uf[i], c("Município", "UF")] <- c("Aurora do Tocantins", "TO")
# Long -71 ao invés de -73
i <- 4
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 2
# lat -19 ao invés de -18
i <- 5
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 1
# Município e UF
i <- 6
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Palmeirópolis", "TO")
# lat e long
i <- 7
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-8.209810, -63.900985)
# Município e UF
i <- 8
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Ananás", "TO")
# lat, long, Município e UF
i <- 9
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-14.694271, -48.604074)
pf[idx_uf[i], c("Município", "UF")] <- c("Niquelândia", "GO")
# Município e UF
i <- 10
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("", "MA")
# long
i <- 11
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 1
# lat
i <- 12
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 1.5
# lat e long
i <- 13
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-8.622048, -63.995284)
# long
i <- 14
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 2
# long, lat, mun, uf
i <- 15
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-11.932307, -49.292047)
pf[idx_uf[i], c("Município", "UF")] <- c("Formoso do Araguaia", "TO")
# lat
i <- 16
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] + 0.2
# long
i <- 17
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 8
# long
i <- 18
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 2.5
# long
i <- 19
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 2
# long
i <- 20
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 1
# lat, município, UF
i <- 21
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 0.15
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# lat e long
i <- 22
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-21.869856, -42.648774)
# lat e long
i <- 23
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- pf[idx_uf[i], c("y_coord", "x_coord")] + c(0.5, -0.5)
# município e uf
i <- 24
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Aurora do Tocantins", "TO")
# long
i <- 25
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] + 3
# long
i <- 26
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] - 3
# município e UF
i <- 27
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# município e UF, lat e long
i <- 28
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-6.501483, -48.626651)
pf[idx_uf[i], c("Município", "UF")] <- c("Xambioá", "TO")
# município e UF
i <- 29
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# long
i <- 30
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] + 2
# lat
i <- 31
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord")] <- pf[idx_uf[i], c("y_coord")] + 0.5
# lat, long, município, uf
i <- 32
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-18.211775, -39.799376)
pf[idx_uf[i], c("Município", "UF")] <- c("Mucuri", "BA")
# lat e long
i <- 33
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-26.570689, -51.985908)
# lat e long
i <- 34
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-8.624051, -63.993691)
# long
i <- 35
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] + 6
# município e UF
i <- 36
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# lat e hem
i <- 37
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord")] <- pf[idx_uf[i], c("y_coord")] * -1
pf[idx_uf[i], lat_cols[4]] <- "Norte"
# lat e hem
i <- 38
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord")] <- pf[idx_uf[i], c("y_coord")] + 0.2
# long
i <- 39
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] - 1
# long
i <- 40
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] + 3
# lat e long
i <- 41
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-8.623858, -63.993720)
# lat e long
i <- 42
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-8.408119, -63.957611)
# long
i <- 43
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] + 1.5
# lat
i <- 44
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord")] <- pf[idx_uf[i], c("y_coord")] - 1
# long
i <- 45
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] + 5
# long
i <- 46
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("x_coord")] <- pf[idx_uf[i], c("x_coord")] + 2

# Verificar se todos os registros possuem ambas coordenadas x e y.
length(which(!is.na(pf$x_coord)))
length(which(!is.na(pf$y_coord)))
idx <- which(is.na(pf$x_coord) & !is.na(pf$y_coord))
pf[idx, c(lat_cols, long_cols, "Localização.descritiva", "Referência.Bibliográfica", "Título.do.Trabalho")]

# Coordenadas métricas.
# Verificar se todos os registros com Northing possuem Easting 
sum(!is.na(pf$Northing)) == sum(!is.na(pf$Easting))

# Existem registros com valores de Easting < 1000 e Northing < 10000, ou seja, apresentado três dígitos
# decimais. A correção pode ser feita multiplicando os valores por 1000
idx <- which(pf$Northing < 10000)
length(idx)
pf$Northing[idx] <- pf$Northing[idx] * 1000
idx <- which(pf$Easting < 1000)
length(idx)
pf$Easting[idx] <- pf$Easting[idx] * 1000

# As coordenadas UTM são formadas por seis (Easting) e sete (Northing) dígitos. Aqui eu verifico se todas
# as coordenadas UTM respeitam essa regra. 70 registros não respeitam essa regra, a maioria deles do trabalho
# "Inventário das terras em microbacias hidrográficas", desenvolvido em SC, representando 9 municípios.
idx <- which(!is.na(pf$Northing))
n <- apply(pf[idx, c("Easting", "Northing")], 1, nchar)
idx_n <- which(!apply(n, 2, function (x) all(x == c(6, 7))))
length(idx_n)
tmp <- pf[idx[idx_n], 
          c("Easting", "Northing", "Número.PA", "UF", "Título.do.Trabalho", "Localização.descritiva",
            "Município")]
rownames(tmp) <- idx[idx_n]
tmp <- tmp[order(tmp$Município), ]
unique(tmp[, "Município"])

# Camboriú
a <- "Camboriú"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1:2][6, ] <- tmp[tmp$Município == a, 2:1][6, ]
tmp[tmp$Município == a, 1] <- tmp[tmp$Município == a, 1] / 10
points(tmp[tmp$Município == a, 1:2])

# Orleans
a <- "Orleans"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1][-11] <- tmp[tmp$Município == a, 1][-11] / 10
tmp[tmp$Município == a, 2][11] <- mean(tmp[tmp$Município == a, 2][-11])
points(tmp[tmp$Município == a, 1:2])

# Pomerode
a <- "Pomerode"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1] <- tmp[tmp$Município == a, 1] / 10
points(tmp[tmp$Município == a, 1:2])

# Santa Maria da Boa Vista
a <- "Santa Maria da Boa Vista"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=24 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1] <- tmp[tmp$Município == a, 1] * 10
tmp[tmp$Município == a, 1:2] <- tmp[tmp$Município == a, 2:1]
points(tmp[tmp$Município == a, 1:2])

# Santana do Ipanema
a <- "Santana do Ipanema"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=24 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1] <- tmp[tmp$Município == a, 1] * 10
points(tmp[tmp$Município == a, 1:2])

# São João do Oeste
a <- "São João do Oeste"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1] <- tmp[tmp$Município == a, 1] / 10
points(tmp[tmp$Município == a, 1:2])

# Seara
a <- "Seara"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1:2][3, ] <- tmp[tmp$Município == a, 2:1][3, ]
tmp[tmp$Município == a, 1] <- tmp[tmp$Município == a, 1] / 10
points(tmp[tmp$Município == a, 1:2])

# Videira
a <- "Videira"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1] <- tmp[tmp$Município == a, 1] / 10
tmp[tmp$Município == a, 2][2] <- tmp[tmp$Município == a, 2][2] / 10
points(tmp[tmp$Município == a, 1:2])

# Xanxerê
a <- "Xanxerê"
ibge <- getCity(a)
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
tmp[tmp$Município == a, 1:2]
tmp[tmp$Município == a, 1][-3] <- tmp[tmp$Município == a, 1][-3] / 10
tmp[tmp$Município == a, 2][3] <- tmp[tmp$Município == a, 2][3] / 10
points(tmp[tmp$Município == a, 1:2])

# Coordenadas corrigidas
pf[idx[idx_n], c("Easting", "Northing")] <- tmp[as.character(idx[idx_n]), 1:2]

# Conferir se os registros com coordenadas UTM possui dado sobre o fuso.
# Buscar pelo nome da cidade no Google Earth junto de camada contendo os quadrantes UTM.
# http://www.processamentodigital.com.br/wp-content/uploads/2011/08/UTMWorldZone.kml
pf$utm <- NA_character_
idx <- which(!is.na(pf$Northing))
works <- unique(pf[idx, "Título.do.Trabalho"])
#
i <- 1
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 2
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 3
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 4
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=24 +south +datum=WGS84 +units=m +no_defs"
#
i <- 5
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Município"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 6
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Título.do.Trabalho"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
#
i <- 7
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Município"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 8
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Município"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=24 +south +datum=WGS84 +units=m +no_defs"
#
i <- 9
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Município"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 10
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 11
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
#
i <- 12
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 13
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 14
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 15
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 16
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 17
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 18
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 19
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 20
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 21
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
#
i <- 22
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 23
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=24 +south +datum=WGS84 +units=m +no_defs"
#
i <- 24
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 25
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 26
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município", "UF")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 27
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município", "UF")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 28
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município", "UF")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=24 +south +datum=WGS84 +units=m +no_defs"
#
i <- 29
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município", "UF")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 30
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município", "UF")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 31
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, c("Localização.descritiva", "Município", "UF")][id_utm, ]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"

# Converter coordenadas métricas para grau decimal.
idx_crs <- unique(pf[idx, "utm"])
for (i in 1:length(idx_crs)) {
  j <- which(pf$utm == idx_crs[i])
  tmp <- pf[j, c("Easting", "Northing")]
  sp::coordinates(tmp) <- c("Easting", "Northing")
  sp::proj4string(tmp) <- sp::CRS(idx_crs[i])
  tmp <- sp::spTransform(tmp, states@proj4string)
  pf[j, c("x_coord", "y_coord")] <- tmp@coords
}

# Visualizar se todos os pontos se encontram dentro do território nacional
# Dois registros estão fora.
sp::plot(states, asp = 1, axes = T)
points(pf[idx, c("x_coord", "y_coord")])
text(pf[idx, c("x_coord", "y_coord")], labels = idx, pos = 4)
#
pf[2272, 
   c("Easting", "Northing", "UF", "Título.do.Trabalho", "Localização.descritiva", "Município", "Número.PA",
     "Classificação.Original")]
pf[2272, "Northing"] <- 7656359
#
pf[689, 
   c("Easting", "Northing", "UF", "Título.do.Trabalho", "Localização.descritiva", "Município", "Número.PA",
     "Classificação.Original")]
pf$Northing[pf$Município == "Santana do Ipanema"]
pf[689, "Northing"] <- 8967805

# Rodar novamente conversão de coordenadas para todos os registros, agora com os dois anteriores corrigidos.
# Uma nova verificação do posicionamento de todos os registros dentro do território nacional será feita ao
# final do exercício, quando coordenadas de outras bases de dados forem adicionadas.
idx_crs <- unique(pf[idx, "utm"])
for (i in 1:length(idx_crs)) {
  j <- which(pf$utm == idx_crs[i])
  tmp <- pf[j, c("Easting", "Northing")]
  sp::coordinates(tmp) <- c("Easting", "Northing")
  sp::proj4string(tmp) <- sp::CRS(idx_crs[i])
  tmp <- sp::spTransform(tmp, states@proj4string)
  pf[j, c("x_coord", "y_coord")] <- tmp@coords
}

# Identificar registros sem coordenadas. Existem quase 3 mil registros sem coordenadas. É muita coisa!!!
idx <- which(is.na(pf$x_coord))
length(idx)
length(pf$x_coord)

# Identificar registros do projeto RADAMBRASIL
pf$radam <- FALSE
pf$radam[grep("radambrasil", pf$Título.do.Trabalho, ignore.case = TRUE)] <- TRUE

# Carregar dados compilados por pesquisadores da Esalq
# A atibuição de coordenadas se dá com base, primeiro, na identificação dos trabalhos por ano conforme a base
# de dados da Esalq.
file <- "fe0003/esalq.csv"
esalq <- read.csv(
  paste("data/raw/", file, sep = ""), head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
esalq <- esalq[order(esalq$PubYear), ]
anos <- unique(esalq$PubYear)
# Levantamento de Reconhecimento dos Solos do Estado de São Paulo (Contribuição à Carta de Solos do Brasil).
# Comissão de Solos. Boletim do Serviço Nacional de Pesquisas Agronômicas (nº 12) . Rio de Janeiro, Ministério
# da Agricultura, Centro Nacional de Ensino e Pesquisas Agronômicas. 1960, 634p.
i <- 1
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), "Source"])
unique(pf[idx[j], "Referência.Bibliográfica"])
l <- which(esalq$PubYear == anos[i]);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"])
cbind(pf[idx[j], "Número.PA"], esalq[which(esalq$PubYear == anos[i]), "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[which(esalq$PubYear == anos[i]), "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <-
  esalq[which(esalq$PubYear == anos[i]), c("longitude", "latitude")][k, ]
# Levantamento Exploratório dos Solos da Região Sob Influência da Cia. Vale do Rio Doce.
i <- 2
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número")])
n <- 13
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i]);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"])
cbind(pf[idx[j], "Número.PA"], esalq[which(esalq$PubYear == anos[i]), "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[which(esalq$PubYear == anos[i]), "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <-
  esalq[which(esalq$PubYear == anos[i]), c("longitude", "latitude")][k, ]
# LEVANTAMENTO EXPLORATÓRIO - RECONHECIMENTO DE SOLOS DO ESTADO DO RIO GRANDE DO NORTE
i <- 3
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
n <- 21
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"])
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# Ministério da Agricultura - Departamento Nacional de Pesquisa Agropecuária: Divisão de Pesquisa Pedológica.
# Levantamento de reconhecimento dos solos do sul do Estado de Mato Grosso. Bolteim técnico, n.18, 839p, 1970.
j <- which(pf$Número[idx] == 18);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
n <- 18
j <- which(pf$Ano.de.Publicação[idx] == 1970 & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- stringr::str_split(pf[idx[j], "Número.PA"], " ", 2, simplify = TRUE)
pa <- apply(pa, 1, function (x) as.numeric(x)[!is.na(as.numeric(x))])
k <- match(pa, esalq[l, "OrgProfID"])
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# LEVANTAMENTO EXPLORATÓRIO - RECONHECIMENTO DE SOLOS DO ESTADO DE PERNAMBUCO - VOLUME II
i <- 4
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número")])
n <- 26
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# Levantamento exploratório - Reconhecimento de solos do Estado do Ceará
i <- 5
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação")])
n <- 28
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- gsub("P", "", pf[idx[j], "Número.PA"])
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 1.
n <- 1
j <- which(pf$Número[idx] == n);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 2.
n <- 2
j <- which(pf$Número[idx] == n);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 3.
n <- 3
j <- which(pf$Número[idx] == n);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 4.
i <- 6
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação")])
n <- 4
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 5.
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação")])
n <- 5
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 8.
i <- 7
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 8
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 7.
n <- 7
pf$Número[which(pf$Código.Trabalho == 707)] <- n
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 12.
i <- 8
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 12
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# LEVANTAMENTO EXPLORATÓRIO - RECONHECIMENTO DE SOLOS\nDA MARGEM ESQUERDA DO RIO SÃO FRANCISCO-ESTADO DA BAHIA.
n <- 38
j <- which(pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
unique(pf[idx[j], c("Referência.Bibliográfica", "Número", "Ano.de.Publicação", "Código.Trabalho")])
j <- which(pf$Número[idx] == n & pf$Ano.de.Publicação[idx] == 1975);length(j)
pa <- stringr::str_split(pf[idx[j], "Número.PA"], "-")
pa <- sapply(pa, function (x) ifelse(length(x) == 2, paste("E", x[1], sep = ""), x))
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 13.
i <- 9
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 13
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 15.
n <- 15
j <- which(pf$Número[idx] == n & pf$radam[idx]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
pa <- gsub("PERFIL Nº ", "", pf[idx[j], "Número.PA"]) 
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 16.
n <- 16
j <- which(pf$Número[idx] == n & pf$radam[idx]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
pa <- gsub("PERFIL Nº ", "", pf[idx[j], "Número.PA"])
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 17.
i <- 10
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 17
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 18.
n <- 18
pf$Número[which(pf$Código.Trabalho == 753)] <- n
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# LEVANTAMENTO DE RECONHECIMENTO DOS SOLOS DO ESTADO DO ESPIRITO SANTO
n <- 45
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- gsub("P", "", pf[idx[j], "Número.PA"])
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# Levantamento Exploratório - Reconhecimento de Solos do Norte de MInas Gerais (Área de Atuação da SUDENE)
i <- 11
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 60
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- gsub("Perfil ", "", pf[idx[j], "Número.PA"])
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# LEVANTAMENTO EXPLORATÓRIO - RECONHECIMENTO DE SOLOS DA MARGEM DIREITA DO RIO SÃO FRANCISCO. ESTADO DA BAHIA -
# VOLUME II
n <- 52
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- stringr::str_trim(gsub("Perfil ", "", pf[idx[j], "Número.PA"]))
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 20.
i <- 12
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 20
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 21.
i <- 13
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 21
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(as.numeric(pf[idx[j], "Número.PA"]), esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 22.
n <- 22
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(as.numeric(pf[idx[j], "Número.PA"]), esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 23.
n <- 23
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
pa <- gsub("P", "", esalq[l, "OrgProfID"])
k <- match(pf[idx[j], "Número.PA"], pa);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# Levantamento de Reconhecimento Dos Solos do Estado do Paraná
i <- 16
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 27
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- gsub("Perfil nº", "", pf[idx[j], "Número.PA"])
pa <- gsub("PC nº", "C", pa)
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# Levantamento Exploratório-Reconhecimento de Solos do Estado do Maranhão
i <- 17
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 35
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- gsub("Perfil ", "", pf[idx[j], "Número.PA"])
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# Levantamento Exploratório-Reconhecimento de Solos do Estado do Piauí
n <- 36
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
pa <- gsub("Perfil ", "", pf[idx[j], "Número.PA"])
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]

# Identificar quantos registros ainda estão sem coordenadas.
# Ainda são 828 perfis com dados de ferro mas sem coordenadas.
idx <- which(is.na(pf$x_coord));length(idx)
length(idx)/nrow(pf)

# Carregar dados pós-processados do SISB (versão 01) e fundir com os dados processados até aqui.
# Os dados são salvos em um arquivo CSV para uso posterior.
tmp <- read.csv(
  "data/raw/fe0003/embrapa-pos-01.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
nrow(tmp)
tmp <- merge(
  pf[-idx, 
     c("Código.PA", "x_coord", "y_coord", "observation_date", "land_use", "litology", "Município", "UF")], 
  tmp, by = "Código.PA", all = TRUE)
nrow(tmp)
tmp$x_coord <- ifelse(!is.na(tmp$x_coord.y), tmp$x_coord.y, tmp$x_coord.x)
tmp$y_coord <- ifelse(!is.na(tmp$y_coord.y), tmp$y_coord.y, tmp$y_coord.x)
tmp$observation_date <- ifelse(!is.na(tmp$observation_date.y), tmp$observation_date.y, tmp$observation_date.x)
tmp$land_use <- ifelse(!is.na(tmp$land_use.y), tmp$land_use.y, tmp$land_use.x)
tmp$litology <- ifelse(!is.na(tmp$litology.y), tmp$litology.y, tmp$litology.x)
tmp$Município <- ifelse(!is.na(tmp$Município.x), tmp$Município.x, tmp$Município.y)
tmp$UF <- ifelse(!is.na(tmp$UF.x), tmp$UF.x, tmp$UF.y)
tmp <- tmp[, -(2:11)]
tmp <- tmp[, !colnames(tmp) %in% c("UF.x", "UF.y", "Município.x", "Município.y")]
sum(is.na(tmp$x_coord))
str(tmp)
sp::plot(states, asp = 1, axes = T)
points(tmp[, c("x_coord", "y_coord")], cex = 0.5)
write.table(tmp, file = "data/raw/fe0003/embrapa-pos-02.csv", sep = ";", fileEncoding = "UTF-8")
