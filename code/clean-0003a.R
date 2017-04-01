# Contribuição 0003
# Responsável: Sistema de Informação de Solos Brasileiros
# Instituição: Embrapa Informática Agropecuária / Embrapa Solos

# Com esse script são processados todos os registros que contenham, obrigatoriamente, algum dado de ferro e o 
# código de identificação do perfil [Código PA] no SISB.

# Preparar ambiente de trabalho
rm(list = ls())
source("code/helper.R")
source("code/helper-utf8.R", encoding = "UTF-8")

# Carregar dados
file <- "fe0003/embrapa.csv"
db <- read.csv(
  paste("data/raw/", file, sep = ""), head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

# Identificar linhas e colunas contendo dados de ferro
id_col <- colnames(db)[grep("Fe", colnames(db))]
id_col
id_col <- id_col[-2]
idx <- which(!is.na(db[, id_col]), arr.ind = TRUE)
id_col <- id_col[unique(idx[, 2])]
id_row <- unique(idx[, 1])
db <- db[id_row, ]

# Calcular tamanho da contribuição
# A contribuição é avaliada por unidade federativa. Contudo, existem registros sem informação da unidade
# federativa. A primeira estratégia usada para atribuir tal informação a esses registros consiste em
# verificar se os demais perfis associados ao mesmo trabalho estão na mesma situação. Quando os demais perfis 
# de um trabalho possuem informação sobre a unidade federativa, verifica-se se ela é a mesma para todos. Nesse
# caso, a unidade federativa única é atribuída aos registros daquele trabalho sem tal informação. 
# Curiosamente, os registros sem informação sobre a unidade federativa também não possuem informação sobre o
# município, localização descritiva, 
db$UF[which(db$UF == "")] <- NA_character_
na_uf <- which(is.na(db$UF))
id_code <- db$Código.Trabalho[na_uf]
id_code <- lapply(id_code, function (x) db$UF[which(db$Código.Trabalho %in% x)])
id_code <- lapply(id_code, unique)
id_code <- lapply(id_code, na.exclude)
db$UF[na_uf] <- sapply(id_code, function (x) ifelse(length(x) == 1, x, NA_character_))
db[na_uf, c("Título.do.Trabalho", "UF")]

# Registros restantes sem informação da unidade federativa.
# Trata-se de um único perfil (48) do 'PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 10.',
# descrito no município de Nova Olinda do Norte, estado do Amazonas (AM).
# Fonte: "http://biblioteca.ibge.gov.br/visualizacao/livros/liv24027.pdf"
na_uf <- which(is.na(db$UF))
db[na_uf, c("Título.do.Trabalho", "Referência.Bibliográfica", "Número.PA", "Símbolo.Horizonte")]
db$UF[na_uf] <- "AM"

# Salvar arquivo com descrição da contribuição para publicação no website.
# Aqui eu ignoro quaisquer alterações na UF decorrentes da criação de novas UFs.
ctb <- data.frame(
  Nome = "Sistema de Informação de Solos Brasileiros",
  Instituição = "Embrapa Informática Agropecuária / Embrapa Solos",
  UF = levels(as.factor(db$UF)),
  Contribuição = summary(as.factor(db$UF)),
  Tipo = "PEDOLÓGICO",
  url = "https://www.bdsolos.cnptia.embrapa.br/consulta_publica.html")
rownames(ctb) <- NULL
ctb
write.csv(ctb, "./web/data/ctb0003.csv", fileEncoding = "UTF-8")  
rm(ctb)

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
# Os dados sobre a litologia subjacente parecem estar todos bem. Devido à grande diversidade de litologias,
# opta-se por não fazer qualquer alteração no sentido de agregação ou padronização. A única ação é a definição
# de NA para os registros sem tal dado.
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
# todos os registros o hemisfério é definido como sendo 'Oeste'. 
idx <- which(pf[, long_cols[4]] == "Leste")
pf[idx, c(long_cols[4], "Código.PA", "UF")]
pf[idx, long_cols[4]] <- "Oeste"
pf[, long_cols[4]] <- ifelse(pf[, long_cols[4]] == "", NA_character_, pf[, long_cols[4]])
# Latitude
# Inúmeros registros possuem 'Norte' como hemisfério, apesar de não terem sido observados nos estados
# do AM, AP, PA ou RR. O valor do hemisfério desses registros é alterado para 'Sul'. 
idx <- which(pf[, lat_cols[4]] == "Norte" & !sapply(pf$UF, function (x) x %in% c("AP", "AM", "PA", "RR")))
pf[idx, c(lat_cols[4], "Código.PA", "UF")]
pf[idx, lat_cols[4]] <- "Sul"
pf[, lat_cols[4]] <- ifelse(pf[, lat_cols[4]] == "", NA_character_, pf[, lat_cols[4]])

# Conferir se há valores de latitude e longitude maiores do que os maiores valores absolutos de latitude
# e longitude do território brasileiro. Em princípio, todos os valores de latitude deveriam ser menores do que 
# aproximadamente 34º S, ou seja, a latitude mais ao sul do teritório brasileiro. Da mesma forma, todos os
# valores de longitude deveriam ser inferiores a aproximadamente 74º O.
# Latitude
# A maioria dos erros correspondem à registros de observações realizadas no estado do Acre. Nestes, o valor de
# latitude pode ser corrigido dividindo-o por 10. A efetividade da estratégia foi verificada conferindo se o
# novo valor de latitude cai dentro do intervalo de valores de latitude da unidade federativa onde a 
# observação foi feita. Os dois registros para os quais a estratégia não funcionou referem-se à observações 
# feitas em MG e RJ, os quais são anotados para futura conferência.
idx <- which(pf[, lat_cols[1]] > bb_br[2, 1])
length(idx) != 0
pf[idx, c(lat_cols[1], "UF", "Código.PA")]
uf_id <- unique(pf$UF[idx])
bb <- lapply(uf_id, function (x) abs(sp::bbox(states@polygons[states$uf == x][[1]])["y", ]))
names(bb) <- uf_id
bb <- lapply(bb, function (x) {
  x[1] <- ceiling(x[1])
  x[2] <- floor(x[2])
  x
})
in_uf <- 
  apply(pf[idx, c(lat_cols[1], "UF")], 1, function (x) {
    tryCatch(hist(as.numeric(x[1]) / 10, breaks = bb[[x[2]]], plot = FALSE)$counts, error = function (e) 0)
  }) == 1
cbind(pf[idx, c(lat_cols[1], "UF")], in_uf)
pf[idx[in_uf], lat_cols[1]] <- pf[idx[in_uf], lat_cols[1]] / 10
out_uf_lat <- idx[!in_uf]
# Longitude
# Similar aos valores de latitude, todos os valores de longitude com erros puderam ser corrigidos dividindo-se
# a longitude por 10. Mais uma vez, todos os registros com erros referem-se a observações feitas no Acre.
idx <- which(pf[, long_cols[1]] > bb_br[1, 1])
length(idx) != 0
pf[idx, c(long_cols[1], "UF", "Código.PA")]
uf_id <- unique(pf$UF[idx])
bb <- abs(sp::bbox(states@polygons[states$uf == uf_id][[1]])[1, ])
in_uf <- 
  sapply(pf[, long_cols[1]][idx] / 10, function (x) {
    tryCatch(hist(x, breaks = bb, plot = FALSE)$counts, error = function (e) 0)
  }) == 1
pf[idx[in_uf], long_cols[1]] <- pf[idx[in_uf], long_cols[1]] / 10
# Também é sabido que, em princípio, todos os valores de longitude deveriam ser maiores do que aproximadamente
# 28º O. Três registros apresentam erros, dois dos quais referindo-se a observações feitas no RJ e MG. Aquele
# descrito no RJ corresponde ao registro acima cujo erro no valor de latitude não pode ser corrigido. 
# Aparentemente os valores de latitude e longitude estão trocados.
idx <- which(pf[, long_cols[1]] < bb_br[1, 2])
length(idx) != 0
pf[idx, c(long_cols[1], "UF", "Código.PA")]
i <- out_uf_lat[out_uf_lat %in% idx]
pf[i, c(lat_cols, long_cols)] # latitude e longitude trocados
pf[i, c(lat_cols, long_cols)] <- pf[i, c(long_cols[-4], lat_cols[-4])]
# Conferir os registros restantes
idx <- c(idx, out_uf_lat)
idx <- idx[idx != i]
# O primeiro deles consiste no perfil 'Exame 17', do 'Estudo Expedito de Solos da Região do Alto Paranaíba,
# Para Fins de Classificação, Correlação e Legenda Preliminar', cuja descrição está disponível em 
# http://ainfo.cnptia.embrapa.br/digital/bitstream/item/62945/1/CNPS-BOL.-TEC.-64-80.pdf. O erro encontra-se
# na digitação da longitude que, ao invés de 7º, é 47º.
pf[idx[1], c("Título.do.Trabalho", "UF", "Número.PA", "Código.Trabalho")]
pf[idx[1], c(lat_cols, long_cols)]
pf[idx[1], long_cols[1]] <- 47
# O segundo refere-se a um perfil do 'Levantamento Exploratório-Reconhecimento de Solos do Estado 
# do Piauí PI', cuja descrição está disponível em http://mapoteca.cnps.embrapa.br. A partir da localização 
# descritiva, vê-se no mapa de solos que a latitude está correta, e que a longitude pode ser corrigida 
# adicionando um 4 à esquerda do 1, ou seja, 41° O.
pf[idx[2],
   c("Título.do.Trabalho", "Número.PA", "Código.Trabalho", "Classificação.Original", "Localização.descritiva",
     lat_cols[1:2], long_cols[1:2])]
pf[idx[2], long_cols[1]] <- 41
# O terceiro consiste de perfil do 'PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 32.'
# descrito em Poços de Caldas, MG. Verificando a localização geográfica do município, nota-se que o valor de
# longitude está correto. Contudo, o valor de latitude, ao invé de 51° S, é 21° S.
pf[idx[3],
   c("Título.do.Trabalho", "Número.PA", "Código.Trabalho", "Classificação.Original", "Localização.descritiva",
      "Ano.de.Publicação", lat_cols[1:2], long_cols[1:2])]
pf[idx[3], lat_cols[1]] <- 21

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
# Dois registros permanecem sem dado sobre o hemisfério. O primeiro foi descrito em Boa Vista (RR), município 
# localizado no hemisfério norte. O segundo consiste em perfil descrito no município de Manacapuru (AM), 
# localizado no hemisfério sul.
idx <- which(is.na(pf[, lat_cols[4]]) & !is.na(pf[, lat_cols[1]]))
pf[idx, c("Título.do.Trabalho", "Código.Trabalho", "Município", "Localização.descritiva",
          lat_cols[1:2], long_cols[1:2])]
pf[idx[1], lat_cols[4]] <- "Norte"
pf[idx[2], lat_cols[4]] <- "Sul"
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
# Valores de minutos e segundos superiores a 60 nunca ocorrem no mesmo registro. Além disso, para a maioria 
# dos registros, o erro vem da fonte, ou seja, do respectivo relatório do levantamento de solos. Como não
# é possível verificar o valor correto, é necessário encontrar uma alternativa robusta e viável. Uma delas 
# consiste em assumir que falta uma vírgula separando os dígitos desses valores de minutos e segundos. Assim,
# a solução consiste em dividir o valor por 10 iterativamente até que o mesmo seja < 60.
# Latitude
idx <- apply(pf[, lat_cols[2:3]], 2, function (x) x > 60)
any(idx, na.rm = TRUE)
idx <- which(idx, arr.ind = TRUE)
any(duplicated(idx[, 1])) # não há registros duplicados
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
idx <- apply(pf[, long_cols[2:3]], 2, function (x) x > 60)
any(idx, na.rm = TRUE)
idx <- which(idx, arr.ind = TRUE)
any(duplicated(idx[, 1])) # não há registros duplicados
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
# Há 17 registros fora do território brasileiro, cinco deles localizados no município de São Gabriel da 
# Cachoeira-AM, todos eles incluídos no "PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 11.".
# O erro parece ser eliminado ao reduzir em dois graus a latitude. Isso faz com que todos os registros 
# permaneçam dentro de território brasileiro e, talvez um detalhe importante na região amazônica, próximo de 
# rodovias e/ou cursos de água. Talvez seja necessário verificar se o mesmo problema acontece nos demais
# registros do volume 11 do RADAMBRASIL.
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
#   a <- pf[i[j], c("y_coord", "x_coord")]
#   a[1] <- a[1] - 2
#   googlemaps(a)
# }
pf[i, "y_coord"] <- pf[i, "y_coord"] - 2
# Demais municípios
i <- idx_out[which(pf[idx_out, "Município"] != "São Gabriel da Cachoeira")]
pf[i, c("Município", "y_coord", "x_coord")]
# Lat -19º ao invés de -24°.
j <- 1
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] + 5
# Long -42° ao invés de -40°.
j <- 2
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] - 2
# Lat -4° ao invés de 4°.
j <- 3
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] * -1
# Long -60° ao invés de -65°.
j <- 4
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 5
# Lat -21° ao invés de -29°.
j <- 5
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] + 8
# Lat -51° ao invés de -61°.
j <- 6
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] + 10
# Registro fora do município de origem
j <- 7
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], c("y_coord", "x_coord")] <- c(-23.16677, -44.833)
# Lat -20° ao invés de -28°.
j <- 8
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] + 8
# Long -45° ao invés de -42°.
j <- 9
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] - 3
# Município Búzios ao invés de Rio de Janeiro + Pequeno deslocamento da observação.
j <- 10
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "Município"] <- "Búzios"
pf[i[j], "x_coord"] <- -41.90844
# Long -42° ao invés de -40°.
j <- 11
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "x_coord"] <- pf[i[j], "x_coord"] - 2
# Lat -8° ao invés de -3°.
j <- 12
pf[i[j], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[i[j], c("y_coord", "x_coord")])
pf[i[j], "y_coord"] <- pf[i[j], "y_coord"] - 5

# Conferir se todos os registros estão dentro da respectiva unidade federativa.
# A maioria dos erros econtram-se próximos dos limites dos estados. O mesmo número de erros foi identificado
# usando o datum SAD69 no lugar de WGS84.
idx_uf <- cbind(pf$UF[idx], tmp)
idx_uf <- which(idx_uf[, 1] != idx_uf[, 2])
length(idx_uf) # numero de registros fora da respectiva unidade federativa.
idx_uf <- idx[idx_uf]
points(pf[idx_uf, c("x_coord", "y_coord")], col = "blue")
# Lat -29 ao invés de -27
i <- 1
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 2
# Long -40 ao invés de -41
i <- 2
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 1
# Lat e Long
i <- 3
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-14.012809, -46.216380)
# Lat e Long
i <- 4
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-9.392922, -40.416417)
# Lat -21.56 ao invés de -21.5
i <- 5
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- -21.56
# Lat -23 ao invés de -22
i <- 6
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 1
# Lat -19 ao invés de -18
i <- 7
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 1
# UF e município
i <- 8
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Número.PA")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
# duckduckgo(pf[idx_uf[i], "Título.do.Trabalho"])
pf[idx_uf[i], c("Município", "UF")] <- c("Ananás", "TO")
# Município e UF
i <- 9
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Número.PA")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Riachinho", "TO")
# UF é TO e Município é Taguatinga
i <- 10
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Taguatinga", "TO")
# UF é TO
i <- 11
pf[idx_uf[i], 
   c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho", "Número.PA")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Aurora do Tocantins", "TO")
# Long -41 ao invés de -42
i <- 12
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 1
# Lat e Long
i <- 13
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-21.282884, -42.257897)
# Lat e Long
i <- 14
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-14.476550, -46.016962)
# Lat -18 ao invés de -12
i <- 15
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 6
# Lat e Long
i <- 16
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-21.492869, -42.256870)
# Long -63 ao invés de -61
i <- 17
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 2
# Long -63 ao invés de -61
i <- 18
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 2
# Lat 1 ao invés de 0
i <- 19
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] + 1
# Long -47 ao invés de -45
i <- 20
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 2
# UF TO
i <- 21
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# Long -43 ao invés de -39
i <- 22
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 4
# Lat e Long
i <- 23
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-9.394322, -40.414474)
# Lat e Long
i <- 24
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-15.513648, -47.966034)
# Hemisfério norte ao invés de sul
i <- 25
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] * -1
# Long -48 ao invés de -47
i <- 26
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 1
# Lat -14 ao invés de -19
i <- 27
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] + 5
# Lat -19 ao invés de -15
i <- 28
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] - 4
# Lat e Long
i <- 29
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-3.676268, -42.878436)
# Long -46 ao invés de -49
i <- 30
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 3
# Lat e Long
i <- 31
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-21.869645, -42.637431)
# Long -45 ao invés de -47
i <- 32
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Classificação.Original")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 2
# UF é SP
i <- 33
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
# Lat e Long + UF é TO
i <- 34
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# Lat e Long
i <- 35
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-14.762755, -46.295042)
# Lat e Long
i <- 36
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-14.928926, -46.156044)
# Long -45 ao invés de -48
i <- 37
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 3
# Duvidoso
i <- 38
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
# Lat e Long. UF é TO
i <- 39
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-6.501672, -48.621753)
pf[idx_uf[i], c("Município", "UF")] <- c("Xambioá", "TO")
# UF é TO
i <- 40
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Palmeirópolis", "TO")
# Lat e Long
i <- 41
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-9.406920, -65.986413)
# Long -42 ao invés de -45
i <- 42
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 3
# Long -41 ao invés de -42
i <- 43
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 1
# Long -47 ao invés de -40
i <- 44
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 7
# Long -55 ao invés de -66
i <- 45
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 11
# Lat -7 ao invés de -8
i <- 46
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] + 1
# Lat -7 ao invés de -8
i <- 47
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- pf[idx_uf[i], "y_coord"] + 1
# Long -56 ao invés de -60
i <- 48
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 4
# Lat -9.4
i <- 49
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "y_coord"] <- -9.4
# Long -63 ao invés de -61
i <- 50
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 2
# Lat e Long
i <- 51
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-12.490311, -45.202049)
# Lat e Long
i <- 52
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-26.568209, -51.984918)
# Lat e Long
i <- 53
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-10.319517, -36.604792)
# Lat e Long
i <- 54
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-15.106748, -48.249891)
# Long -46 ao invés de -52
i <- 55
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 6
# Lat e Long
i <- 56
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-5.527408, -40.924353)
# Long -70 ao invés de -69
i <- 57
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 1
# UF é PE + Município é Petrolina
i <- 58
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
# Lat -43 ao invés de -39
i <- 59
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 4
# Lat -55 ao invés de -58
i <- 60
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 3
# UF é TO
i <- 61
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Alvorada do Tocantins", "TO")
# Long -42.28333 ao invés de -42.48333
i <- 62
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- -42.28333
# Long -64 ao invés de -65
i <- 63
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 1
# Lat e Long
i <- 64
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-8.638501, -63.995155)
# Lat, Long e UF
i <- 65
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-12.849453, -46.566630)
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# Long -64 ao invés de -66
i <- 66
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] + 2
# UF é TO
i <- 67
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Alvorada", "TO")
# UF e Município
i <- 68
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("Município", "UF")] <- c("Cristalina", "GO")
# Lat, Long, UF e Município
i <- 69
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord")] <- pf[idx_uf[i], c("y_coord")] - 0.7
pf[idx_uf[i], c("Município", "UF")] <- c("Arraias", "TO")
# Lat e Long
i <- 70
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-13.006054, -46.624292)
# Long -62 ao invés de -60
i <- 71
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], "x_coord"] <- pf[idx_uf[i], "x_coord"] - 2
# Lat e Long
i <- 72
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-21.909039, -42.733607)
# UM e Município
i <- 73
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
# Lat e Long
i <- 74
pf[idx_uf[i], c("y_coord", "x_coord", "UF", "Município", "Localização.descritiva", "Título.do.Trabalho")]
# googlemaps(pf[idx_uf[i], c("y_coord", "x_coord")])
pf[idx_uf[i], c("y_coord", "x_coord")] <- c(-1.124704, -46.523179)
#

# Verificar se todos os registros possuem coordenadas x e y.
# Há um registro na cidade de Macaé. Não consegui encontrar o original. Lat e Long foram estimados.
length(which(!is.na(pf$x_coord)))
length(which(!is.na(pf$y_coord)))
idx <- which(is.na(pf$x_coord) & !is.na(pf$y_coord))
pf[idx, c(lat_cols, long_cols, "Localização.descritiva", "Referência.Bibliográfica", "Título.do.Trabalho")]
db[which(db$Localização.descritiva == pf$Localização.descritiva[idx]), c(lat_cols, long_cols)]
pf[idx, c("y_coord", "x_coord")] <- c(-22.354978, -41.808248)

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
# as coordenadas UTM respeitam essa regra. Dez registros não respeitam essa regra, todos eles nos estados
# de AL, PR, RJ e SC, muitos proveninentes do mesmo trabalho.
idx <- which(!is.na(pf$Northing))
n <- apply(pf[idx, c("Easting", "Northing")], 1, nchar)
idx_n <- which(!apply(n, 2, function (x) all(x == c(6, 7))))
length(idx_n)
tmp <- pf[idx[idx_n], 
          c("Easting", "Northing", "Número.PA", "UF", "Título.do.Trabalho", "Localização.descritiva",
            "Município")]
rownames(tmp) <- idx[idx_n]
tmp <- tmp[order(tmp$UF), ]
# Santana do Ipanema
ibge <- getCity("Santana do Ipanema")
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=24 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, asp = 1, axes = TRUE)
# Northing 8966161 ao invés de 896161
i <- 1
tmp[i, ]
points(674545, 8966161, col = i)
tmp[i, 2] <- 8966161
# Easting 686832 ao invés de 68632
i <- 2
tmp[i, ]
points(686832, 8965443, col = i)
tmp[i, 1] <- 686832
# São Mateus do Sul
ibge <- getCity("São Mateus do Sul")
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(
  ibge, sp::CRS("+proj=utm +zone=22 +south +ellps=aust_SA +towgs84=-57,1,-41,0,0,0,0 +units=m +no_defs"))
sp::plot(ibge, axes = TRUE)
i <- 3
tmp[i, ]
tmp[i, 1:2] <- NA
# Nova Friburgo
ibge <- getCity("Nova Friburgo")
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(ibge, sp::CRS("+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"))
sp::plot(ibge, axes = T)
i <- 4
tmp[i, ]
points(752185, 7523324)
tmp[i, 1:2] <- c(752185, 7523324)
i <- 5
tmp[i, ]
points(754766, 7536414)
tmp[i, 1:2] <- c(754766, 7536414)
# Rio de Janeiro
i <- 6
tmp[i, ]
tmp[i, 1] <- tmp[i, 1] * 10
# Rio Claro
i <- 7
tmp[i, ]
tmp[i, 1] <- 605741
# Lontras
ibge <- getCity("Lontras")
ibge <- swapAxisOrder(ibge)
ibge <- sp::spTransform(ibge, sp::CRS("+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"))
sp::plot(ibge, axes = T)
i <- 8
tmp[i, ]
points(650930, 6991184, col = "red")
tmp[i, 1] <- 650930
i <- 9
tmp[i, ]
points(652382, 6995250)
tmp[i, 1] <- 652382
i <- 10
tmp[i, ]
points(650485, 6990015)
tmp[i, 1:2] <- c(650485, 6990015)
# Coordenadas corrigidas
pf[idx[idx_n], c("Easting", "Northing")] <- tmp[as.character(idx[idx_n]), 1:2]

# Conferir se os registros com coordenadas UTM possui dado sobre o fuso.
# Curiosamente a base de dados não exportou o fuso. A solução é verificar o número de trabalhos que contém
# coordenadas UTM, e então buscar o fuso na fonte. Isso deve ser fácil, pois são apenas 13 trabalhos. Em 
# alguns casos, informações sobre o sistema de coordenadas de referência são dadas na descrição dos perfis.
# A maneira mais fácil foi conferindo o nome da cidade no Google Earth junto de camada contendo os quadrantes
# UTM.
pf$utm <- NA_character_
idx <- which(!is.na(pf$Northing))
works <- unique(pf[idx, "Título.do.Trabalho"])
#
i <- 1
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 2
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 3
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
#
i <- 4
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 5
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 6
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=24 +south +datum=WGS84 +units=m +no_defs"
#
i <- 7
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=24 +south +datum=WGS84 +units=m +no_defs"
#
i <- 8
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 9
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 10
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
#
i <- 11
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
#
i <- 12
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
pf[idx, "utm"][id_utm] <- "+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs"
#
i <- 13
works[i]
id_utm <- which(pf[idx, "Título.do.Trabalho"] == works[i])
pf[idx, "Localização.descritiva"][id_utm]
tmp <- pf[idx, "Localização.descritiva"][id_utm]
tmp <- lapply(tmp, function (x) {
  out <- unlist(stringr::str_split(stringr::str_trim(stringr::str_split_fixed(x, "UTM:", n = 2)[2]), " "))
  out <- as.numeric(out)
  out[which(!is.na(out))[1]]
})
tmp <- paste("+proj=utm +zone=", tmp, " +south +datum=WGS84 +units=m +no_defs", sep = "")
pf[idx, "utm"][id_utm] <- tmp

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
# Três registros estão fora.
sp::plot(states, asp = 1, axes = T)
points(pf[idx, c("x_coord", "y_coord")])
text(pf[idx, c("x_coord", "y_coord")], labels = idx)
#
pf[4488, 
   c("Easting", "Northing", "UF", "Título.do.Trabalho", "Localização.descritiva", "Município", "Número.PA",
     "Classificação.Original")]
pf[4488, "Northing"] <- 7656359
# 
pf[1394, 
   c("Easting", "Northing", "UF", "Título.do.Trabalho", "Localização.descritiva", "Município", "Número.PA",
     "Classificação.Original")]
pf$Northing[pf$Município == "Santana do Ipanema"]
pf[1394, "Northing"] <- 8967805
#
pf[6023, 
   c("Easting", "Northing", "UF", "Título.do.Trabalho", "Localização.descritiva", "Município", "Número.PA",
     "Classificação.Original", "Código.Trabalho")]
pf[which(pf$Código.Trabalho == 778), "Northing"]
pf[6023, "Northing"] <- 7644576
# Rodar novamente conversão de coordenadas para todos os registros, agora com os três anteriores corrigidos.
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
sp::plot(states, asp = 1)
points(pf[idx[j], c("x_coord", "y_coord")])
# Levantamento Exploratório dos Solos da Região Sob Influência da Cia. Vale do Rio Doce.
i <- 2
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número")])
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 13);length(j)
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
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 21);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == 21);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"])
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# Ministério da Agricultura - Departamento Nacional de Pesquisa Agropecuária: Divisão de Pesquisa Pedológica.
# Levantamento de reconhecimento dos solos do sul do Estado de Mato Grosso. Bolteim técnico, n.18, 839p, 1970.
j <- which(pf$Número[idx] == 18);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
j <- which(pf$Ano.de.Publicação[idx] == 1970 & pf$Número[idx] == 18);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == 18);length(l)
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
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 26);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == 26);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# LEVANTAMENTO DE RECONHECIMENTO DE MÉDIA INTENSIDADE DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DA
# MARGEM DIREITA DO RIO PARANÁ - ESTADO DE GOIÁS.
j <- which(pf$Número[idx] == 23);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceType", "PubYear")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
j <- which(pf$Ano.de.Publicação[idx] == 1983 & pf$Número[idx] == 23);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == 23);length(l)
pa <- stringr::str_split(pf[idx[j], "Número.PA"], " ", Inf, simplify = TRUE)[, -1]
pa[, 1] <- ifelse(pa[, 1] == "COM.", "C", "")
pa[, 2] <- gsub("N", "", pa[, 2]) 
pa <- apply(pa, 1, function (x) paste(x[1], x[2], sep = ""))
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
# Levantamento exploratório - Reconhecimento de solos do Estado do Ceará
i <- 5
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação")])
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 28);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == 28);length(l)
pa <- gsub("P", "", pf[idx[j], "Número.PA"])
k <- match(pa, esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 3.
j <- which(pf$Número[idx] == 3);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Referência.Bibliográfica", "Número")])
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 3 & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == 3);length(l)
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
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 4 & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == 4);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 5.
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação")])
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 5 & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == 5);length(l)
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
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == 8 & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == 8);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
cbind(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"][k])
cbind(pf[idx[j], "Classificação.Original"], esalq[l, "SiBCS1998"][k])
pf[idx[j], c("x_coord", "y_coord")] <- esalq[l, c("longitude", "latitude")][k, ]
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 9.
n <- 9
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
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 16.
n <- 16
j <- which(pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(pf[idx[j], "Número.PA"], esalq[l, "OrgProfID"]);sum(!is.na(k))
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
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 29.
i <- 14
anos[i]
j <- which(pf$Ano.de.Publicação[idx] == anos[i]);length(j)
unique(esalq[which(esalq$PubYear == anos[i]), c("Source", "SourceNumber", "SourceVolume", "SourceType")])
unique(pf[idx[j], c("Título.do.Trabalho", "Número", "Ano.de.Publicação", "Código.Trabalho")])
n <- 29
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n & pf$radam[idx]);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceVolume == n);length(l)
k <- match(as.numeric(pf[idx[j], "Número.PA"]), esalq[l, "OrgProfID"]);sum(!is.na(k))
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
# LEVANTAMENTO DE RECONHECIMENTO DOS SOLOS E AVALIAÇÃO DA APTIDÃO AGRÍCOLA DAS TERRAS DE UMA ÁREA DE 
# COLONIZAÇÃO NO MUNICÍPIO DE CAREIRO, ESTADO DO AMAZONAS.
n <- 31
j <- which(pf$Ano.de.Publicação[idx] == anos[i] & pf$Número[idx] == n);length(j)
l <- which(esalq$PubYear == anos[i] & esalq$SourceNumber == n);length(l)
k <- match(as.numeric(pf[idx[j], "Número.PA"]), esalq[l, "OrgProfID"]);sum(!is.na(k))
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

# Identificar quantos registros ainda estão sem coordenadas.
# Ainda são 2 mil perfis com dados de ferro mas sem coordenadas.
idx <- which(is.na(pf$x_coord));length(idx)
length(idx)/nrow(pf)

# Recarregar dados do SISB e fundir com os dados processados até aqui.
# Os dados são salvos em um arquivo CSV para uso posterior.
tmp <- read.csv(
  "data/raw/fe0003/embrapa.csv", head = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
nrow(tmp)
tmp <- merge(
  pf[-idx, 
     c("Código.PA", "x_coord", "y_coord", "observation_date", "land_use", "litology", "Município", "UF")], 
  tmp, by = "Código.PA", all = TRUE)
nrow(tmp)
tmp$Município <- ifelse(!is.na(tmp$Município.x), tmp$Município.x, tmp$Município.y)
tmp$UF <- ifelse(!is.na(tmp$UF.x), tmp$UF.x, tmp$UF.y)
tmp <- tmp[, !colnames(tmp) %in% c("UF.x", "UF.y", "Município.x", "Município.y")]
sum(is.na(tmp$x_coord))
str(tmp)
sp::plot(states, asp = 1, axes = T)
points(tmp[, 2:3], cex = 0.5)
write.table(tmp, file = "data/raw/fe0003/embrapa-pos-01.csv", sep = ";", fileEncoding = "UTF-8")




















# Ainda é muita coisa!!! Contudo, nem mesmo a
# base de dados da Esalq possui tais informações. Numa rápida visita ao SISB, verifiquei que, para vários 
# perfis, as coordenadas simplesmente não foram digitadas. Em alguns casos elas foram inseridas junto da 
# descrição da localização geográfica. Da mesma forma, há inúmeros perfis para os quais os dados de ferro
# não foram digitados. Talvez seja mais apropriado processar todos os dados de todos os trabalhos, primeiro 
# numa escala de trabalho maior, como é o caso aqui, depois passando para o ajuste fino, trabalho por 
# trabalho.

write.csv(pf, "data/raw/fe0003/tmp-profiles-a.csv", fileEncoding = "UTF-8")

# 
# 
# 
# 
# 
# 
# 
# 
# 
# #
# 
# 
# 
# 
# 
# 
# # Uma estratégia possível é atribuir coordenadas 
# # aleatoriamente aos pontos dentro dos limites dos municípios onde foram obtidos. Assim, quanto maior for o
# # município, maior será o erro posicional.
# # Atribuir coordenada aleatória
# # xy <- list()
# # for (i in 1:10) {
# #   cat(unlist(pf[idx, c("Município", "UF")][i, ]), "...\n")
# #   ibge <- getCity(pf$Município[idx][i])
# #   ibge <- sp::spTransform(ibge, sp::proj4string(states))
# #   if (length(ibge) == 1) {
# #     ibge <- swapAxisOrder(ibge)
# #     xy[[i]] <- c(sp::spsample(ibge, 1, "random")@coords, pf[idx, c("Município", "UF")][i, ])
# #   } else {
# #     tmp <- as.data.frame(sp::coordinates(ibge))
# #     sp::coordinates(tmp) <- ~ V2 + V1
# #     sp::proj4string(tmp) <- sp::proj4string(states)
# #     ibge$uf <- sp::over(tmp, states)
# #     ibge <- ibge[which(ibge@data$uf == pf$UF[idx][i]), ]
# #     ibge <- swapAxisOrder(ibge)
# #     xy[[i]] <- c(sp::spsample(ibge, 1, "random")@coords, pf[idx, c("Município", "UF")][i, ])
# #   }
# # }
# # xy <- do.call(rbind, xy)
# # 
# # 
# # sp::plot(states, asp = 1, axes = TRUE)
# # points(xy[, 1:2], col = 1, pch = 20)
# # text(xy[, 1:2], labels = apply(xy[, 3:4], 1, function (x) paste(x[1], " (", x[2], ")", sep = "")), pos = 4,
# #      cex = 0.75)
# # 
# # 
# # 
# # sp::plot(states, asp = 1, axes = TRUE)
# # points(xy, col = 2)
# 
# 
# 
# 
# 
# 
# 
# 
# # Conferindo os dados de alguns dos perfis sem coordenadas nos relatórios de origem, percebeu-se que muitos
# # deles pode estar vindo de levantamentos antigos, onde não havia coordenadas. Isso significa que pode haver
# # perfis repetidos na base de dados. Assim pode não ser confiável usar a fonte do dado relatada na base de
# # dados. Uma alternativa pode ser usar a UF e informações da classificação taxonômica do perfil.
# idx <- which(is.na(pf$x_coord))
# length(idx)
# 
# # Atribuir informação da UF para dados da Esalq
# tmp <- esalq[, c("latitude", "longitude")]
# sp::coordinates(tmp) <- c("latitude", "longitude")[2:1]
# sp::proj4string(tmp) <- sp::proj4string(states)
# tmp <- sp::over(tmp, states)
# esalq$UF <- tmp
# rm(tmp)
# uf <- unique(pf$UF)
# 
# # PA
# i <- 9
# uf[i]
# work <- unique(
#   pf[idx, ][pf$UF[idx] == uf[i], c("Título.do.Trabalho", "Tipo.de.Publicação", "Referência.Bibliográfica")])
# nrow(work)
# ## 
# j <- 7
# work[j, ]
# pa <- pf[idx, ][pf$UF[idx] == uf[i] & pf$Título.do.Trabalho[idx] == work$Título.do.Trabalho[j], "Número.PA"]
# pa
# tmp <- esalq[
#   # esalq$SourceType == "BT" &
#   # esalq$Source == "SNLCS" &
#     # esalq$PubYear == 1980 &
#     # esalq$SourceNumber == 33 &
#     # esalq$SourceVolume == 8 &
#     esalq$UF == uf[i] &
#     !is.na(esalq$esalq_id)
#   , ]
# tmp[, c("PubYear", "Source", "SourceType", "SourceNumber", "SourceVolume", "UF")]
# pa;tmp[, "OrgProfID"]
# 
# 
# pa <- gsub("Perfil ", "", pa)
# k <- match(pa, tmp[, "OrgProfID"]);sum(!is.na(k))
# na.exclude(cbind(pa, tmp[, "OrgProfID"][k]))
# 
# write.csv(na.exclude(
#   cbind(
#     pf[idx, ][pf$UF[idx] == uf[i] & pf$Título.do.Trabalho[idx] == work$Título.do.Trabalho[j], c("Número.PA", "Classificação.Original")], tmp[k, c("OrgProfID", "SiBCS1998")])
# ), "data/raw/fe0003/tmp.csv", fileEncoding = "UTF-8")
# 
# 
# dim(pf)
# 
# 
# ##
# sp::plot(states, asp = 1, axes = TRUE)
# points(esalq[, c("longitude", "latitude")], cex = 0.5, col = 1)
# points(pf[, c("x_coord", "y_coord")], cex = 0.5, pch = 20, col = 2)
# #
# 
# 
# 
# lapply(pa, function (x) {
#   n <- lapply(tmp[, "SiBCS1998"], stringr::str_split_fixed, " ", Inf)
#   n <- floor(mean(sapply(n, length)))
#   x <- paste(stringr::str_split_fixed(x, pattern = " ", n = n)[1:(n - 3)], collapse = " ")
#   agrep(x, tmp[, "SiBCS1998"], ignore.case = TRUE)
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#   
#   
#  
# 
# 
# # 
# 
# 
# pf[idx, c("Título.do.Trabalho", "Número")][which(pf[idx, "radam"] & pf[idx, "Ano.de.Publicação"] == 1973), ]
# 
# i <- which(esalq$Source == "RADAM" & esalq$SourceVolume == 1)
# 
# 
# esalq[i, c("PubYear", "OrgProfID", "SiBCS1998")]
# 
# # # Identify RADAM profiles without coordinates
# # 
# # na_lat <- which(is.na(pf$latitude[idx_radam]))
# # 
# # title <- stringr::str_trim(pf$Título.do.Trabalho[idx_radam])
# # title <- stringr::str_split_fixed(string = title, pattern = "Volume ", n = 2)[, 2]
# # title <- as.numeric(gsub(pattern = ".", replacement = "", x = title, fixed = TRUE))
# # 
# # idx_title <- which(title == 14)
# # 
# # pf$Ano.de.Publicação[idx_radam][idx_title]
# # 
# # i <- 1
# # idx_year <- which(pf$Ano.de.Publicação[na_lat][i] == esalq$PubYear, arr.ind = TRUE)
# # # idx_tax <- agrep(
# #   # stringr::word(pf$Classificação.Original[na_lat][i], end = 5), 
# #   # stringr::word(esalq$SiBCS1998[idx_year], end = 5),
# #   # ignore.case = TRUE, max.distance = 0.2
# # # )
# # 
# # if (pf$radam[na_lat][i]) {
# #   
# # } else {
# #   if (pf$Tipo.de.Publicação[na_lat][i] == "Boletim técnico") {
# #     idx <- which(esalq$SourceType[idx_year] == "BT")
# #     esalq[idx_year, ][idx, ]
# #   }
# # }
# # 
# # 
# # 
# # 
# # 
# # idx_tax
# # 
# # esalq[idx_year, ][idx_tax, ]
# # pf[na_lat, ][i, ]
# # 
# # 
# # 
# # 
# # 
# # # idx_source <- which(pf$source == "RADAM")
# # 
# # i <- 4
# # idx_year <- which(pf$Ano.de.Publicação[idx_source][i] == esalq$PubYear, arr.ind = TRUE)
# # idx_vol <- which(pf$Número[idx_source][i] == esalq$SourceVolume[idx_year], arr.ind = TRUE)
# # 
# # charmatch(pf$Número.PA[idx_source][i], esalq$OrgProfID[idx_vol])
# # 
# # idx_pf <- which(as.numeric() == , arr.ind = TRUE)
# # 
# # 
# # 
# # esalq[idx_pf, ][idx_year, ][idx_vol, ]
# # pf[i, ]
# # 
# # 
# # 
# # # Get data source volume
# # idx <- grep("volume", pf$Título.do.Trabalho, ignore.case = TRUE)
# # volume <- stringr::str_split_fixed(
# #   pf$Título.do.Trabalho[idx], stringr::fixed("volume ", ignore_case = TRUE), n = 2)[, 2]
# # volume <- stringr::word(volume, 1L)
# # volume <- gsub(".", "", volume, fixed = TRUE)
# # volume <- gsub(")", "", volume, fixed = TRUE)
# # volume <- ifelse(is.na(as.integer(volume)), as.integer(as.roman(volume)), as.integer(volume))
# # na_vol <- which(is.na(volume))
# # volume2 <- stringr::str_split_fixed(
# #   pf$Título.do.Trabalho[idx[na_vol]], stringr::fixed("- ", ignore_case = TRUE), n = 2)[, 2]
# # volume2 <- stringr::word(volume2, 1L)
# # volume2 <- as.integer(as.roman(volume2))
# # volume[na_vol] <- volume2
# # pf$volume <- NA_integer_
# # pf$volume[idx] <- volume
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # head(esalq)
# # summary(as.factor(esalq$Source))
# # 
# # # 
# # tmp <- db[, c("Northing", "Easting")]
# # sp::coordinates(tmp) <- ~ Northing + Easting
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # Soil classification
# # pf$classe <- paste(pf$Classe.de.Solos.Nível.3, " ", pf$X1ª.Ocorrência, sep = "")
# # Encoding(pf$classe) <- "UTF-8"
# # pf$sibcs <- sapply(pf$classe, sibcsSymbol)
# # 
# # 
# # 
# # 
# # # Save temporary file
# # write.csv(pf, "data/raw/fe0002/tmp.csv")
# # rm(pf)
# # 
# # # Horizon ####
# # hz <- db[id_row, c(
# #   "Código.PA", "Código.Horizonte", "Símbolo.Horizonte", "Profundidade.Superior", "Profundidade.Inferior", 
# #   id_col)]
# # str(hz)
# # 
# # # Save temporary file
# # write.csv(hz, "data/raw/fe0002/tmp.csv")
# # 
# # # Remove temporary file
# # system("rm data/raw/fe0002/tmp.csv")
