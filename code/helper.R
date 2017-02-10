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
  function (x) {
    
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
    z <- first_level[grep(y[1], first_level[, 2], ignore.case = TRUE), 1][1]
    
    # Second level
    second_level <- c(
      "A", "amarelo", 
      "Y", "flúvico",
      "X", "háplico",
      "V", "vermelho",
      "VA", "vermelho-amarelo")
    second_level <- t(matrix(second_level, nrow = 2))
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
    z <- paste(z, third_level[grep(y[3], third_level[, 2], ignore.case = TRUE), 1][1], sep = "")
    
    return (z)
  }
