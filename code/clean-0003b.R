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
googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
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
googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
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
