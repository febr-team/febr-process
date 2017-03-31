# Classificação do uso da terra ####
keys <- list(
  'AGRICULTURA' = c(
    "arroz", "algodão", "aveia", "abacaxi", "agave", "arrado",
    "banana", "batata", "bananeira", "batatinha",
    "cupuaçu", "cana-de-açúcar", "citrus", "castanha", "coco", "café", "cultura", "cacau", "cafezal",
    "coqueiro", "cultivo", "caju", "cultura de subsistência", "cultura anual", "centeio", "capoeira",
    "desmatado", "derrubada",
    "extrativismo", "experimentação", "erva-mate",
    "fruticultura", "feijão", "fumo", "fava",
    "girassol", "gradeada",
    "horticultura",
    "juta",
    "lavoura", "laranja",
    "mata explorada", "mata explorada",
    "mata em exploração", "mata em exploração", 
    "mamona", "mandioca", "milho", "macaxeira", "manga",
    "olericultura",
    "pomar", "pousio", "plantio", "piaçava", "pimenta", "preparo para plantio",
    "roça",
    "sapotí", "soja", "sisal",
    "trigo", "tomate",
    "vinhedo"
  ),
  'CRIAÇÃO ANIMAL' = c(
    "azevém", "algaroba",
    "bovino", "braquiária",
    "capim", "campo nativo", "capim-colonião", "criação", "extensiva", "capim-gordura", 
    "capim-elefante", "caprino", "capim-angola", "capim-guiné", "capim sempre-verde", "capim-jaraguá",
    "campo natural",
    "desmatado", "derrubada",
    "equino", "extensiva",
    "forragem",
    "gado", "gramínea", "grama",
    "leite",
    "ovino",
    "palma", "pastagem", "pecuária", "pasto", "pecuária extensiva", "pastagem natural", "pecuária na caatinga",
    "pastagem artificial", "pastagem plantada", "pecuária no cerrado", "pecuária extensiva no cerrado"
  ),
  # 'OUTRO' = c(
  # ),
  'PROTEÇÃO DA NATUREZA' = c(
    "cobertura vegetal natural", "cerrado", "cobertura natural", "caatinga",
    "floresta", "floresta remanescente", "floresta nativa",
    "mata natural", "mata secundária", "mata atlântica", "mata",
    "nenhum", "não utilizado", "não aproveitado", "não constatado", "não utilizado agrícolamente",
    "não observado",
    "parque nacional",
    "reserva florestal", "reserva", 
    "savana", "sem utilização", "sem uso", "sem uso agrícola", "sem uso aparente",
    "vegetação natural", "vegetação secundária", "vegetação nativa"
  ),
  'SILVICULTURA' = c(
    "carvão",
    "eucalipto",
    "florestamento",
    "horto florestal",
    "lenha",
    "madeira", 
    "pinus", "pinheiro", "plantio de eucalipto",
    "reflorestamento", "reflorestamento de pinus", "reflorestamento de eucalipto",
    "seringueira"
  ),
  'URBANO' = c(
    "área de empréstimo", "extração areia", "área urbana",
    "exploração mineral",
    "loteamento",    
    "mineração",
    "recreação",
    "urbana"
  )
)
guessClass <-
  function (obj, keywords, max.distance = 0.1) {
    out <- 
      sapply(keywords, function (x) {
        rowSums(
          sapply(x, function (y) {
            agrepl(pattern = y, x = obj, ignore.case = TRUE, max.distance = max.distance)
          })
        )
      })
    out <- apply(out, 1, function (x) round(x / sum(x), 4))
    return (out)
  }
# Fazer busca na Internet usando https://www.google.com.br/maps ####
googlemaps <-
  function (x) {
    x <- paste("https://www.google.com.br/maps/place/@", x[1], ",", x[2], 
               ",10z/data=!3m1!4b1!4m5!3m4!1s0x0:0x0!8m2!3d", x[1], "!4d", x[2], "?hl=en", 
               sep = "")
    browseURL(x)
  }
# Fazer busca na Internet usando https://duckduckgo.com ####
duckduckgo <-
  function (x) {
    x <- paste("https://duckduckgo.com/?q=", x,"&t=canonical&ia=web", sep = "")
    browseURL(x)
  }
# Buscar limites municipais no geoservidor do IBGE #### 
getCity <- 
  function (cityname = "Espumoso") {
    tmp <- tempfile(fileext = ".shp")
    dsn <- paste(
      "WFS:http://www.geoservicos.ibge.gov.br:80/geoserver/wfs?service=WFS&version=1.1.0&",
      "request=GetFeature&typeName=CGEO:LIM_Municipios2013", sep = "")
    gdalUtils::ogr2ogr(
      src_datasource_name = dsn, dst_datasource_name = tmp, 
      where = paste("nommunic='", cityname, "'", sep = ""))
    rgdal::readOGR(dsn = tmp, stringsAsFactors = FALSE)
  }
swapAxisOrder <-
  function (x) {
    l <- length(x)
    for (i in 1:l) {
      x@polygons[[1]]@Polygons[[i]]@coords <- x@polygons[[1]]@Polygons[[i]]@coords[, 2:1]
    }
    x@bbox <- t(apply(x@polygons[[1]]@Polygons[[i]]@coords, 2, range))
    x
  }
# Transform coordinates in degrees, minutes, and decimal seconds to decimal degrees ####
dms2dd <- 
  function (x, type = "lat") {
    
    x[, 1:3] <- lapply(1:3, function (i) ifelse(is.na(x[, i]), NA_real_, x[, i]) )
    m <- x[, 2] / 60
    s <- x[, 3] / 3600
    res <- x[, 1] + ifelse(is.na(m), 0, m) + ifelse(is.na(s), 0, s)
    
    for (i in 1:length(res)) {
      if (!is.na(x[i, 4])) {
        if (type == "lat") {
          if (x[i, 4] == "Sul") {
            res[i] <- res[i] * -1
          } else if (x[i, 4] == "Norte") {
            res[i] <- res[i]
          } else {
            res[i] <- NA_real_
          }
        } else {
          if (x[i, 4] == "Oeste") {
            res[i] <- res[i] * -1
          } else if (x[i, 4] == "Leste") {
            res[i] <- res[i]
          } else {
            res[i] <- NA_real_
          }
        }
      }
    }
    return (res)
  }

# Get symbol from soil classification
sibcsSymbol <- 
  function (x, encoding = "UTF-8") {
    
    y <- unlist(strsplit(x = x, split = " ", fixed = TRUE))
    
    # First level
    first_level <- c(
      "P", "argissolo",
      "C", "cambissolo",
      "M", "chernossolo",
      "E", "espodossolo",
      "G", "gleissolo", 
      "L", "latossolo",
      "T", "luvissolo", 
      "R", "neossolo", 
      "N", "nitossolo", 
      "O", "organossolo", 
      "S", "planossolo",
      "P", "plintossolo", 
      "V", "vertissolo")
    first_level <- t(matrix(first_level, nrow = 2))
    Encoding(first_level) <- encoding
    z <- first_level[grep(y[1], first_level[, 2], ignore.case = TRUE), 1][1]
    
    # Second level
    second_level <- c(
      "A", "amarelo", 
      "Y", "flúvico",
      "X", "háplico",
      "V", "vermelho",
      "VA", "vermelho-amarelo")
    second_level <- t(matrix(second_level, nrow = 2))
    Encoding(second_level) <- encoding
    z <- paste(z, second_level[grep(y[2], second_level[, 2], ignore.case = TRUE), 1][1], sep = "")
    
    # Third level
    if (y[3] %in% c("Ta", "Tb")) {
      clay_cec <- c(
        "v", "Ta",
        "b", "Tb"
      )
      clay_cec <- t(matrix(clay_cec, nrow = 2))
      z <- paste(z, clay_cec[grep(y[3], clay_cec[, 2], ignore.case = TRUE), 1][1], sep = "")
      y <- y[-3]
    }
    third_level <- c(
      "al", "alítico",
      "a", "alumínico",
      "df", "distroférrico",
      "d", "distrófico",
      "e", "eutrófico",
      "o", "órtico",
      "q", "psalmítico"
    )
    third_level <- t(matrix(third_level, nrow = 2))
    Encoding(third_level) <- encoding
    z <- paste(z, third_level[grep(y[3], third_level[, 2], ignore.case = TRUE), 1][1], sep = "")
    
    return (z)
  }
