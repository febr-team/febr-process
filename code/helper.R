# Transformação de sistema de coordenadas de referência ####
spTransform0 <- 
  function (coord_x, coord_y, crs_src, crs_dst = "EPSG:4326") {
    y <- sp::SpatialPoints(
      matrix(c(coord_x, coord_y), nrow = 1), 
      proj4string = sp::CRS(paste("+init=", tolower(crs_src), sep = "")))
    y <- sp::spTransform(y, sp::CRS(paste("+init=", tolower(crs_dst), sep = "")))
    y$coord_system <- crs_dst
    as.data.frame(y)
  }

# Calcular contribuições e gerar metadados para website ####
createSiteMetadata <-
  function (n, dataset, observation, layer, metadata, sharing) {
    
    # Preparar URL
    docs_sheet <- "https://docs.google.com/spreadsheets/d/"
    drive_folder <- "https://drive.google.com/open?id="
    dataset <- paste(docs_sheet, dataset, sep = "")
    observation <- paste(docs_sheet, observation, sep = "")
    layer <- paste(docs_sheet, layer, sep = "")
    metadata <- paste(docs_sheet, metadata, sep = "")
    sharing <- paste(drive_folder, sharing, sep = "")
    
    # Descarregar dados
    dataset <- read.csv(
      text = gsheet::gsheet2text(dataset, format = 'csv'), stringsAsFactors = FALSE, dec = ",")
    observation <- read.csv(
      text = gsheet::gsheet2text(observation, format = 'csv'), stringsAsFactors = FALSE, dec = ",")
    layer <- read.csv(
      text = gsheet::gsheet2text(layer, format = 'csv'), stringsAsFactors = FALSE, dec = ",")
    
    # Agregar dados das observações e camadas
    db <- merge(observation, layer, by = "observation_id")
    
    # Identificar linhas e colunas contendo dados de ferro
    # Gerar metadados apenas se realmente houver dados de ferro
    id_col <- colnames(db)[grep("^fe_", colnames(db))]
    if (length(id_col) > 0) {
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
        Instituição =
          stringr::str_split_fixed(dataset[dataset$item == "organization_name", 2], ";", n = Inf)[1],
        Título = dataset[dataset$item == "dataset_title", 2],
        UF = levels(as.factor(db[id_row, "state_id"])),
        Contribuição = summary(as.factor(db[id_row, "state_id"])),
        Tipo = ifelse(
          dataset[dataset$item == "subject", "data"] == "Gênese, Morfologia e Classificação dos Solos",
          "PEDOLÓGICO", "EDAFOLÓGICO"),
        url = sharing)
      rownames(ctb) <- NULL
      print(ctb)
      
      # Salvar arquivo com descrição da contribuição para publicação no website
      write.csv(ctb, paste("./web/data/", n, ".csv", sep = ""), fileEncoding = "UTF-8")
    } else {
      cat("Não há dados de ferro")
    }
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
